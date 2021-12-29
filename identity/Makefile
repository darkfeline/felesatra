identity.txt: keys.txt message.txt
	gpg -o $@ --yes --clearsign message.txt
	printf "\n" >>$@
	cat keys.txt >>$@
