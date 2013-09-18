SRCDIR=$PWD
cd ~
ln -s $SRCDIR/lib lib
echo 'source ~/lib/profile' >> ~/.bashrc
ln -s $SRCDIR/.emacs.d .emacs.d
ln -s $SRCDIR/.tmux.conf .tmux.conf
ln -s $SRCDIR/.gitconfig .gitconfig
ln -s $SRCDIR/.vimrc .vimrc
