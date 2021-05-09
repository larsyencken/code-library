serve:
	raco pollen start

open:
	open http://localhost:8080/index.ptree

hello.txt: hello.txt.pp
	raco pollen render hello.txt.pp

clean:
	rm -rf compiled hello.txt
