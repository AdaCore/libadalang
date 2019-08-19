from __future__ import absolute_import, division, print_function

import os


# pyflakes off
with_gnatpython = False
if not os.environ.get('WITHOUT_GNATPYTHON'):
    try:
        from gnatpython.env import Env
    except ImportError:
        pass
    else:
        with_gnatpython = True
if not with_gnatpython:
    Env = None
# pyflakes on


additional_discriminants = set()


def add_discriminants(discr_str):
    """
    Import additional discriminants from a comma-separated list.

    :param str discr_str: Comma-separated list of discriminants to add.
    """
    if discr_str:
        additional_discriminants.update(discr_str.split(','))


def get_discriminants():
    """
    If GNATpython is available, use it to return the list of discriminants
    asociated with the curent context (target, host, etc.). Otherwise, return
    an empty list of discriminants.

    :rtype: list[str]
    """
    return ((Env().discriminants if Env else []) +
            sorted(additional_discriminants))


class Matcher(object):

    class Pattern(object):
        def __init__(self, specs, message):
            self.specs = specs
            self.message = message

        @classmethod
        def from_json(cls, json):
            # Patterns must be dicts that contain exactly two keys: "if" and
            # "then".
            if not isinstance(json, dict) or set(json) != {'if', 'then'}:
                raise ValueError('Invalid pattern: {}'.format(json))

            # Value for the "if" key must be a list of specs
            spec_list = json['if']
            spec_result = []
            if isinstance(spec_list, list):
                for spec in spec_list:
                    # These specs must be themselves mere strings, which
                    # represent discriminant names to match. The pattern will
                    # match if all of its specs are satisfied.
                    #
                    # As a special case, an empty list of specs always matches.
                    if isinstance(spec, str):
                        # There are two kinds of specs. Either a plain
                        # discriminant name, in which case the spec matches if
                        # the discrimimant is present. Either a bang (!)
                        # followed by the discriminant name, in which case the
                        # spec matches iff. the discriminant is not present.
                        spec_result.append((True, spec[1:])
                                           if spec.startswith('!') else
                                           (False, spec))
                    else:
                        raise ValueError('Invalid discriminant spec: {}'
                                         .format(spec))

            else:
                raise ValueError('Invalid discriminant spec list: {}'.format(
                    spec_list))

            # Value for the "then" key must be a string
            message = json['then']
            if not isinstance(message, str):
                raise ValueError('Invalid pattern message: {}'.format(message))

            return cls(spec_result, message)

        def matches(self, discriminants):
            """
            If this pattern matches the given list of discriminants, return
            this pattern's message. Otherwise, return None.

            :rtype: None|str
            """
            for not_flag, name in self.specs:
                if not_flag and name in discriminants:
                    return None
                elif not not_flag and name not in discriminants:
                    return None
            return self.message

    def __init__(self, pattern_list):
        """
        :type patterns: list[list[(bool, str)]]
        """
        self.pattern_list = pattern_list

    @classmethod
    def from_json(cls, json):
        # At the top-level, we expect a list of patterns. Discriminants will
        # match if they satisfy at least one of these patterns.
        #
        # As a special case, a mere top-level string designates a message whose
        # pattern always matches.
        if isinstance(json, str):
            return cls([cls.Pattern([], json)])
        elif isinstance(json, list):
            return cls([cls.Pattern.from_json(pattern) for pattern in json])
        else:
            raise ValueError('Invalid list of discriminant patterns: {}'
                             .format(json))

    def matches(self, discriminants=None):
        """
        If this matches the given list of discriminants, return the
        corresponding message. Otherwise, return None.

        If `discriminants` is left to None, this list is computed from the
        environment.

        :rtype: None|str
        """
        if discriminants is None:
            discriminants = set(get_discriminants())

        for pattern in self.pattern_list:
            matched = pattern.matches(discriminants)
            if matched is not None:
                return matched
        return None


# Tests, run when executed as a standalone module
if __name__ == '__main__':
    discrs = ['A', 'B']

    for json, expected_result in [
        ('always', 'always'),
        ([{'if': ['A'], 'then': 'got a'}], 'got a'),
        ([{'if': ['A', 'B'], 'then': 'got a&b'}], 'got a&b'),
        ([{'if': ['A', 'B', 'C'], 'then': 'got a&b&c'}], None),
        ([{'if': ['A', '!C'], 'then': 'got a&!c'}], 'got a&!c'),
        ([{'if': ['!A'], 'then': 'got !a'}], None),

        ([{'if': ['!A'], 'then': 'got !a'},
          {'if': ['A', 'B'], 'then': 'got a&b'}], 'got a&b'),

        ([{'if': ['A'], 'then': 'got a'},
          {'if': ['A', 'B'], 'then': 'got a&b'}], 'got a'),
    ]:
        result = Matcher.from_json(json).matches(discrs)
        assert result == expected_result, (
            'Unexpected result for {}:\n    got {} but {} expected'.format(
                json, repr(result), repr(expected_result))
        )
