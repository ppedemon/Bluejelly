#! /bin/bash

SEEN_MP=0
MP=
ARGS=

for a in $*; do
  case $a in
  "-mp") 
     SEEN_MP=1
     ;;
  *)
     if [ $SEEN_MP -eq 1 ]; then
       SEEN_MP=0
       MP=${MP}:$a
     else
       ARGS=$ARGS" "$a
     fi
  esac           
done

CP=
for jar in `ls lib/*.jar`; do
  CP+=":"$jar
done
CP=${CP:1}${MP}

#CP=lib/brt.jar
#CP=${CP}:lib/asm-3.2.jar
#CP=${CP}:lib/asm-commons-3.2.jar
#CP=${CP}:lib/asm-util-3.2.jar
#CP=${CP}${MP}

#echo "Args are" $ARGS
#echo CP is ${CP}

java -Dapp.version=1.0 -cp ${CP} bluejelly.runtime.Runtime ${ARGS}
exit $?

