from subprocess import Popen, PIPE
import os

with open("/sys/class/power_supply/BAT0/charge_now") as f:
    charge_now = float(f.read())
with open("/sys/class/power_supply/BAT0/charge_full") as f:
    charge_full = float(f.read())

THRESHOLD = 20
percent = 100*charge_now/charge_full
duration = 1
freq = 800


if percent < THRESHOLD:
    p = Popen(['osd_cat','-A','center','-p','middle','-f','-*-*-bold-*-*-*-36-120-*-*-*-*-*-*','-cred','-s','5'],stdin=PIPE)
    p.communicate(input=b"Battery charge is " + bytes(str(int(percent)), encoding='utf8') + b"%!")
    p.wait()
    os.system('play -nq -t alsa synth {} sine {}'.format(duration, freq))
