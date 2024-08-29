-include lib/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url https://github.com/emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/main
	@cd lib/borg; git reset --hard HEAD


profile:
	emacs -Q -l ./profile-dotemacs.el --eval "(setq profile-dotemacs-file \"~/.emacs.d/init.el\")"  -f profile-dotemacs
