import subprocess

def format():
    subprocess.run(["fprettify", "-r", ".", "-i", "4", "-l", "999"])

format()