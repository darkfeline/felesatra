OUTPUTDIR=public

SSH_HOST=www.felesatra.moe
SSH_PORT=22
SSH_USER=root
SSH_TARGET_DIR=/srv/www/www.felesatra.moe

all: build rsync_upload

build:
	hugo

rsync_upload: build
	rsync -e "ssh -p $(SSH_PORT)" -P -rvzc --delete $(OUTPUTDIR)/ $(SSH_USER)@$(SSH_HOST):$(SSH_TARGET_DIR) --cvs-exclude

.PHONY: all build rsync_upload
