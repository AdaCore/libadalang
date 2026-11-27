#!/usr/bin/env python3
"""Download a Libadalang release wheel from Cathod and publish it to PyPI."""

from __future__ import annotations

import json
import os
from pathlib import Path
from subprocess import CalledProcessError
import sys
from urllib.parse import unquote, urlparse

import e3.log
from e3.main import Main
from e3.os.process import Run


log = e3.log.getLogger("publish_pypi")


def run_command(
    command: list[str],
    *,
    cwd: Path | None = None,
    env: dict[str, str] | None = None,
) -> None:
    """Run a command with live output and stop publishing if it fails."""
    process = Run(command, cwd=cwd, env=env, output=None, error=None)
    if process.status != 0:
        raise CalledProcessError(process.status, command)


def pypi_id_token() -> str:
    """Obtain a Google ID token for PyPI, including AWS federation support."""
    import google.auth
    from google.auth import impersonated_credentials
    from google.auth.transport.requests import Request
    from google.oauth2.id_token import fetch_id_token

    with open(os.environ["GOOGLE_APPLICATION_CREDENTIALS"]) as stream:
        info = json.load(stream)

    if info.get("type") == "external_account":
        url = info.pop("service_account_impersonation_url", None)
        if not url:
            raise ValueError(
                "Federated credentials require "
                "service_account_impersonation_url"
            )
        account = unquote(urlparse(url).path).rsplit("/", 1)[-1]
        if not account.endswith(":generateAccessToken"):
            raise ValueError("Invalid service_account_impersonation_url")
        account = account.removesuffix(":generateAccessToken")
        if (
            info.get("subject_token_type")
            == "urn:ietf:params:aws:token-type:aws4_request"
        ):
            from e3.aws import Session

            # The e3 AWS session supports ECS task credentials; Google's
            # loader otherwise falls back to EC2 instance metadata.
            region = "eu-west-1"
            aws_credentials = (
                Session(regions=[region]).to_boto3().get_credentials()
            )
            if aws_credentials is None:
                raise ValueError(
                    "No AWS credentials available for "
                    "Workload Identity Federation"
                )
            aws_credentials = aws_credentials.get_frozen_credentials()
            os.environ["AWS_REGION"] = region
            os.environ["AWS_ACCESS_KEY_ID"] = aws_credentials.access_key
            os.environ["AWS_SECRET_ACCESS_KEY"] = aws_credentials.secret_key
            if aws_credentials.token:
                os.environ["AWS_SESSION_TOKEN"] = aws_credentials.token
            else:
                os.environ.pop("AWS_SESSION_TOKEN", None)
        scopes = ["https://www.googleapis.com/auth/cloud-platform"]
        source, _ = google.auth.load_credentials_from_dict(info, scopes=scopes)
        # Use the federated identity directly to avoid nested impersonation
        target = impersonated_credentials.Credentials(
            source_credentials=source,
            target_principal=account,
            target_scopes=scopes,
        )
        credentials = impersonated_credentials.IDTokenCredentials(
            target, target_audience="pypi", include_email=True
        )
        credentials.refresh(Request())
        return credentials.token
    else:
        return fetch_id_token(Request(), "pypi")


def publish(setup: str) -> None:
    """Download, check and upload the wheel with configured credentials."""
    credentials = os.environ.get("GOOGLE_APPLICATION_CREDENTIALS")
    if not credentials:
        raise ValueError(
            "Set the GOOGLE_APPLICATION_CREDENTIALS File variable"
        )
    with open(credentials):
        pass

    release_dir = Path("pypi-release").resolve()
    release_dir.mkdir(exist_ok=True)
    run_command(
        [
            "e3-cathod",
            "components",
            "--setup",
            setup,
            "--build-date",
            "all",
            "--platform",
            "x86_64-linux",
            "--component",
            "libadalang-internal",
            "--download",
            "--as-name",
            "libadalang-internal",
        ],
        cwd=release_dir,
    )
    component_dir = release_dir / "libadalang-internal"
    component_dir.mkdir(exist_ok=True)
    run_command(
        [
            "tar",
            "xf",
            str(release_dir / "libadalang-internal.tar.gz"),
            "-C",
            str(component_dir),
            "--strip-components=1",
        ],
    )
    wheels = sorted(
        (component_dir / "stripped_python").glob("libadalang-*.whl")
    )
    if not wheels:
        raise FileNotFoundError("No Libadalang release wheels found")

    twine = [sys.executable, "-m", "twine"]
    run_command([*twine, "check", *map(str, wheels)])
    # Twine exchanges the Google ID token for a short-lived PyPI upload token
    env = os.environ.copy()
    env["PYPI_ID_TOKEN"] = pypi_id_token()
    env["TWINE_NON_INTERACTIVE"] = "1"
    run_command(
        [
            *twine,
            "upload",
            "--verbose",
            "--repository-url",
            "https://upload.pypi.org/legacy/",
            *map(str, wheels),
        ],
        env=env,
    )


def main() -> int:
    cli = Main()
    cli.argument_parser.description = __doc__
    cli.argument_parser.add_argument(
        "--setup", required=True, help="Cathod setup to download"
    )
    cli.parse_args()
    assert cli.args is not None
    try:
        publish(cli.args.setup)
    except Exception as exc:
        log.error("%s", exc)
        log.debug("Publishing failed", exc_info=True)
        return exc.returncode if isinstance(exc, CalledProcessError) else 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
