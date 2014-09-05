from os import path
from mako.template import Template
from common import LANGUAGE, c_repr, get_type, null_constant


class TemplateEnvironment(dict):
    """
    Environment that gathers names for template processing.
    Names are associated to values with the attribute syntax.
    """

    def __setattr__(self, name, value):
        self[name] = value

    def __getattr__(self, name):
        return self[name]


class Renderer(object):

    def __init__(self, template_env=None, **kwargs):
        self.env = TemplateEnvironment(template_env or {})
        self.env.update(kwargs)
        # "self" is a reserved name in Mako, so our variables cannot use it.
        # TODO??? don't use "_self" at all in templates. Use more specific
        # names instead.

    def update(self, env):
        return Renderer(self.env, **env)

    def extend(self, **kwargs):
        return Renderer(self.env, **kwargs)

    def render(self, template_name, env=None, **kwargs):
        env = env or {}
        return self.update(
            TemplateEnvironment(
                env.items() + kwargs.items()))._render(template_name)

    def _render(self, template_name):
        return mako_template(template_name).render(**self.env)


template_cache = {}


def mako_template(file_name):
    t_path = path.join(path.dirname(path.realpath(__file__)),
                       "templates", LANGUAGE, file_name + ".mako")
    t = template_cache.get(t_path, None)

    if not t:
        t = Template(strict_undefined=True, filename=t_path)
        template_cache[t_path] = t

    return t


common_renderer = Renderer({
    'c_repr':           c_repr,
    'get_type':         get_type,
    'null_constant':    null_constant,
})
