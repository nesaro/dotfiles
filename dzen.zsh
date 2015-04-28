#!/bin/zsh
 
###
# Config
###
DATE_FORMAT="%a %d %b, %Y"
TIME_ZONES=("Australia/Perth" "Europe/Warsaw")
SEPERATOR=' ^fg(#86AA3F)^c(3)^fg() '
BAR_BG='#7DA926'
BAR_FG='#B9D56E'
BAR_H=7
BAR_W=50
BAR_ARGS="-bg $BAR_BG -fg $BAR_FG -w $BAR_W -h $BAR_H"
ICON_DIR="$HOME/.share/icons/dzen"
 
GLOBALIVAL=1m
DATEIVAL=60
TIMEIVAL=1
 
 
###
# Functions
###
_date()
{
    date +${DATE_FORMAT}
}
 
_time()
{
    local zone
    print_space=0
    for zone in $TIME_ZONES; do
        [[ $print_space -eq 1 ]] && print -n " "
        print -n "${zone:t}: $(TZ=$zone date '+%H:%M')"
        print_space=1
    done
}
 
DATEI=0
TIMEI=0
 
date=$(_date)
times=$(_time)
 
while true; do
    [[ $DATEI -ge $DATEIVAL ]] && date=$(_date) && DATEI=0
    [[ $TIMEI -ge $TIMEIVAL ]] && times=$(_time) && TIMEI=0
 
    print "${SEPERATOR}${times}${SEPERATOR}${date}"
 
    DATEI=$(($DATEI+1))
    TIMEI=$(($TIMEI+1))
 
    sleep $GLOBALIVAL
done
