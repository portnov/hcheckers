
from setuptools import setup

setup(name = 'hcheckers',
        version = '2022.04',
        description = 'HCheckers client application',
        url = 'https://github.com/portnov/hcheckers',
        author = 'Ilya V. Portnov',
        author_email = 'portnov84@ramlber.ru',
        license = 'BSD3',
        packages = ['hcheckers'],
        zip_safe = False,
        scripts = ['hcheckers/hcheckersc.py'],
        include_package_data = True,
        data_files = [
                ('share/applications', ['hcheckers.desktop']),
                ('share/icons/hicolor/scalable/apps', ['hcheckers/icons/hcheckers.svg'])
            ],
        install_requires = [
            'PyQt5'
            ])
