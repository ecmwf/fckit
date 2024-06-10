#!/usr/bin/env python3

# (C) Copyright 2024 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from pathlib import Path
import pytest

from fckit_yaml_reader import YAML
yaml = YAML()


@pytest.fixture(scope='module', name='here')
def fixture_here():
    return Path(__file__).parent


@pytest.fixture(scope='function', name='tmpfile')
def fixture_tmpfile(here):

    tmpfile = here / 'tmp.yml'

    yield tmpfile

    if tmpfile.exists():
        tmpfile.unlink()


@pytest.fixture(scope='module', name='refdict')
def fxiture_refdict():

    return {
        'name': 'type config',
        'types': [
            {'typeA': [
                ['memberA', 'real', 3],
                ['memberB', 'real', 3]
                ]
            },
            {'typeB': [
                ['memberA', 'real', 3],
                ['memberB', 'int', 2]
                ]
            }
        ]
    }


def test_yaml_parse(here, refdict):
    """Test parsing capability of fckit_yaml_reader."""

    with open(here / 'test_config.yml', 'r') as stream:
        _dict = yaml.safe_load(stream)

    assert _dict == refdict


def test_yaml_dump(here, refdict, tmpfile):
    """Test writing capability of fckit_yaml_reader."""

    with open(here / tmpfile, 'w') as stream:
        yaml.dump(refdict, stream)

    with open(here / tmpfile, 'r') as stream:
        _dict = yaml.safe_load(stream)

    assert _dict == refdict
