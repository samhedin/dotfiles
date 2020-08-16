sudo pacman -Syu xclip openssh
ssh-keygen -t rsa -b 4096 -C "sam.hedin@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
xclip -sel clip < ~/.ssh/id_rsa.pub
