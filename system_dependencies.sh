#!/bin/bash 
install_prefix=""
echo "Which package manager do you use?"
select yn in "apt" "dpkg" "pacman" "brew" "zypper" "yum" "dnf" "none"; do
    case $yn in
        apt )  sudo apt update; install_prefix="sudo apt install "; break;;
        pacman ) sudo pacman -Syu; install_prefix="sudo pacman -S "; break;;
        brew ) install_prefix="brew install "; break;;
        zypper ) install_prefix="sudo zypper install "; break;;
        yum ) install_prefix="sudo yum install "; break;;
        dnf ) install_prefix="sudo dnf install "; break;;
        none ) echo "please install one of these package managers or manually install the packages listed in INSTALL.md with your own package installer"; sleep 5; exit;;
    esac
done

$install_prefix pv
$install_prefix openssl
$install_prefix libssl-dev
$install_prefix libplplot-dev
$install_prefix libshp-dev
$install_prefix liblapacke-dev
$install_prefix libopenblas-dev