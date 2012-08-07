#! /bin/bash

CP=
for jar in `ls lib/*.jar`; do
  CP+=":"$jar
done
CP=${CP:1}

java -Dapp.version=1.0 -Dapp.name=bas -cp ${CP} bluejelly.asm.Assembler $@
exit $?

