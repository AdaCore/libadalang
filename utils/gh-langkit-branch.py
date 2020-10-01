#! /usr/bin/env python3

"""
This tool is meant for CIs (GitHub Actions, AppVeyor, ...) to checkout the
proper langkit branch.
"""

import argparse
import subprocess


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('target_repo')
parser.add_argument('target_ref', default='', nargs='?')
parser.add_argument('pr_repo', default='/', nargs='?')
parser.add_argument('pr_ref', default='', nargs='?')

args = parser.parse_args()

pr_author = args.pr_repo.split('/')[0]

# Slug for the pull request to test (for instance:
# some-contributor/libadalang), or an empty string if we test a push build.
pull_request_slug = (
    "{}/libadalang".format(pr_author) if pr_author else ""
)

# Slug for the repository currently being built and the branch name
repo_branch = args.target_ref.split('/')[-1] if args.target_ref else ''

print("REPO SLUG: ", args.target_repo)

# Queue of couples (slug, branch) for the Langkit branches to test. The last
# one is tested first.
trials = [('AdaCore/langkit', 'master')]

# Determine the Libadalang repository/branch that we are testing
if pull_request_slug:
    slug = pull_request_slug
    branch = args.pr_ref
else:
    slug = args.target_repo
    branch = repo_branch

# Then, assuming there is a "langkit" repository that is a sibling of the
# tested one...
langkit_slug = '{}/langkit'.format(slug.split('/')[0])

# Try to build either the same branch, or fall back to AdaCore's master
trials.append((langkit_slug, branch))

for slug, branch in reversed(trials):
    print('Trying to fetch {} (branch {})'.format(slug, branch))
    if subprocess.call([
        'git', 'fetch', 'https://github.com/{}.git'.format(slug), branch
    ]):
        print('   ... failed')
    else:
        break

subprocess.check_call(['git', 'checkout', 'FETCH_HEAD'])
