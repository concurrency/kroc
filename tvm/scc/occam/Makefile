#template: $(APP_NAME).occ
#	touch bytecode.h
#	rm bytecode.h
#	occbuild --program $(APP_NAME).occ
#	tbc2h $(APP_NAME).tbc
#	rm $(APP_NAME) $(APP_NAME).tbc $(APP_NAME).tce
#	mv $(APP_NAME).h bytecode.h

filename = sandbox
mtvm: filename = mtvm
hello: filename = hello
commstime: filename = commstime

sandbox: $(filename).occ
	touch bytecode.h
	rm bytecode.h
	occbuild --program $(filename).occ
	tbc2h $(filename).tbc
	rm $(filename) $(filename).tbc $(filename).tce
	mv $(filename).h bytecode.h
	
mtvm: $(filename).occ
	touch bytecode.h
	rm bytecode.h
	occbuild --program $(filename).occ
	tbc2h $(filename).tbc
	rm $(filename) $(filename).tbc $(filename).tce
	mv $(filename).h bytecode.h
	
hello: $(filename).occ
	touch bytecode.h
	rm bytecode.h
	occbuild --program $(filename).occ
	tbc2h $(filename).tbc
	rm $(filename) $(filename).tbc $(filename).tce
	mv $(filename).h bytecode.h
	
commstime: $(filename).occ
	touch bytecode.h
	rm bytecode.h
	occbuild --program $(filename).occ
	tbc2h $(filename).tbc
	rm $(filename) $(filename).tbc $(filename).tce
	mv $(filename).h bytecode.h
	
