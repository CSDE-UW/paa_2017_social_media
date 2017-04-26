## Install pySocialWatcher
- **pySocialWatcher is currently only compatible with Python2.7**
    * if you downloaded Anaconda for Python3.x you can create a conda environment with Python2.7
        * first type `conda create -n py27 python=2.7` in the terminal to create a Python2.7 environment named py27
        * then type `source activate py27` to activate the py27 environment 
    * setup notebook in py27 environment
        * `conda install notebook ipykernel matplotlib`
        * `ipython kernel install --user`
* install pySocialWatcher with the following commands in your terminal
~~~
 git clone https://github.com/maraujo/pySocialWatcher.git
 cd pySocialWatcher
 pip install -r requirements.txt
 python setup.py install
~~~
* bash script to install pySocialWatcher for OSX users:
* from the "install_software" directory, type `sh download_pySocialWatcher.sh` to install pySocialWatcher
    * default installation is to your home directory
    * to install pySocialWatcher in the directory of your choice, copy "download_pySocialWatcher.sh" to your preferred directory and comment out (insert '#') the first line of "download_pySocialWatcher.sh" before typing `sh download_pySocialWatcher.sh`
* pySocialWatcher [examples and documentation](https://github.com/maraujo/pySocialWatcher)
* [quick tutorial](https://goo.gl/WzE9ic) on pySocialWatcher module