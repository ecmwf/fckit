# coding: utf-8

from __future__ import print_function, absolute_import, division, unicode_literals

if False:  # MYPY
    from typing import Dict, Any  # NOQA

_package_data = dict(
    full_package_name='ruamel.yaml.clib',
    version_info=(0, 2, 12),
    __version__='0.2.12',
    version_timestamp='2024-10-20 11:49:54',
    author='Anthon van der Neut',
    author_email='a.van.der.neut@ruamel.eu',
    description='C version of reader, parser and emitter for ruamel.yaml derived from libyaml',
    license='MIT',
    entry_points=None,
    nested=True,  # not really nested as this should not have any files under ruamel
    binary_only=True,
    since=2019,
    ext_modules=[
        dict(
            name='_ruamel_yaml',
            src=[
                '_ruamel_yaml.c',
                'api.c',
                'writer.c',
                'dumper.c',
                'loader.c',
                'reader.c',
                'scanner.c',
                'parser.c',
                'emitter.c',
            ],
            lib=[],
            test="""
            int main(int argc, char* argv[])
            {
              /* prevent warning */
              return 0;
            }
            """,
        ),
    ],
    # NOQA
    # test='#include "ext/yaml.h"\n\nint main(int argc, char* argv[])\n{\nyaml_parser_t parser;\nparser = parser;  /* prevent warning */\nreturn 0;\n}\n',  # NOQA
    classifiers=[
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Programming Language :: Python :: 3.12',
        'Programming Language :: Python :: 3.13',
        'Programming Language :: Python :: Implementation :: CPython',
        'Topic :: Software Development :: Libraries :: Python Modules',
    ],
    keywords='yaml 1.2 parser c-library config',
    wheels=dict(
        windows='appveyor',
        linux='libyaml-devel',
        macos='builder@macos',
    ),
    # read_the_docs='yaml',
    supported=[(3, 9)],  # minimum
    python_requires='>=3.9',
    tox=dict(
        env='*g',
    ),
    manifest='include README.md LICENSE setup.py *.c *.h *.pxd *.pyx',
    # rtfd='yaml',
)  # type: Dict[Any, Any]


version_info = _package_data['version_info']
__version__ = _package_data['__version__']
