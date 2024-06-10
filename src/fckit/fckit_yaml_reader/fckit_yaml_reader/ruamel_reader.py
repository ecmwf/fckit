#!/usr/bin/env python3

# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from ruamel.yaml import YAML

class ruamel_reader(YAML):
    """
    A minimal wrapper for ruamel.yaml to create an API consistent with pyyaml.
    """

    def __init__(self):
        super().__init__(typ='safe')

    def safe_load(self, stream):
        return self.load(stream)
