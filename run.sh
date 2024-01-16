set -e

if [ -z $ENABLE_PROFILE ];
then
  PROFILE_ARG=""
else
  PROFILE_ARG="-Dprofile"
fi

erlc $PROFILE_ARG collector.erl
erlc $PROFILE_ARG file_loader.erl
erlc $PROFILE_ARG processor.erl
erl -args_file vm.args \
  -s file_loader run $1
  # -s file_loader run "../1brc/measurements.txt"
