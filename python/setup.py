
import os
from setuptools import setup

os.umask(2)

setup(name = 'hcheckers',
        version = '0.1.0.0',
        description = 'HCheckers client application',
        url = 'https://github.com/portnov/hcheckers',
        author = 'Ilya V. Portnov',
        author_email = 'portnov84@ramlber.ru',
        license = 'BSD3',
        packages = ['hcheckers'],
        zip_safe = False,
        scripts = ['hcheckers/hcheckersc.py'],
        include_package_data = True,
        install_requires = [
            'PyQt5'
            ])
