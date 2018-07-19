#! /usr/bin/env python2

import os
import subprocess
import sys


pull_request_slug = os.environ.get('TRAVIS_PULL_REQUEST_SLUG')
pull_request_branch = os.environ.get('TRAVIS_PULL_REQUEST_BRANCH')
repo_slug = os.environ.get('TRAVIS_REPO_SLUG')
repo_branch = os.environ.get('TRAVIS_BRANCH')

for var in ('TRAVIS_PULL_REQUEST_SLUG', 'TRAVIS_PULL_REQUEST_BRANCH',
        'TRAVIS_REPO_SLUG', 'TRAVIS_BRANCH'):
    print >> sys.stderr, '{} = {}'.format(var, os.environ.get(var))


# Queue of couples (slug, branch) for the Langkit branches to test. The last
# one is tested first.
trials = [('AdaCore/langkit', 'master')]

# Determine the Libadalang repository/branch that we are testing
if pull_request_slug:
    slug = pull_request_slug
    branch = pull_request_branch
else:
    slug = repo_slug
    branch = repo_branch

# Then, assuming there is a "langkit" repository that is a sibling of the
# tested one...
langkit_slug = '{}/langkit'.format(slug.split('/')[0])

# Try to build either the same branch, or fall back to master
trials.append((langkit_slug, 'master'))
trials.append((langkit_slug, branch))

for slug, branch in reversed(trials):
    print 'Trying to fetch {} (branch {})'.format(slug, branch)
    if subprocess.call([
        'git', 'fetch', 'https://github.com/{}.git'.format(slug), branch
    ]):
        print('   ... failed')
    else:
        break

# subprocess.check_call(['git', 'checkout', 'FETCH_HEAD'])
print('done!')
