#!/usr/bin/env python
from setuptools import setup, find_packages
import os

# build ec
os.system('make -C ..')
os.system('cp ../str ./ec_str')
os.system('cp ../list ./ec_list')

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
