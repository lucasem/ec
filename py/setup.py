#!/usr/bin/env python
from setuptools import setup, find_packages
import os

# build ec
_ROOT = os.path.abspath(os.path.dirname(__file__))
rel = lambda path: os.path.join(_ROOT, path)
os.system(f"make -C {rel('..')}")
os.system(f"cp {rel('../str')} {rel('ec_str')}")
os.system(f"cp {rel('../list')} {rel('ec_list')}")

# build package
setup(
    name='ecalgorithm',
    version='1.0.0',
    description='ec_algorithm is a wrapper around a binary of the exploration-compression algorithm',
    author='Lucas E. Morales',
    author_email='lucas@lucasem.com',
    classifiers=[
            'Programming Language :: Python :: 3',
        ],
    packages=find_packages(),
    package_data={
        'ecalgorithm': ['ec_str', 'ec_list']
    },
)
