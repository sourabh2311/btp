import os
import sys
import glob
import difflib

inputAskingTestCases = set(["tc3.tig", "tc5.tig", "tc6.tig", "tc8.tig"])

changeDir = "../Compiler"
oldDir = os.getcwd()
os.chdir(changeDir)
correctTestCasesDir = "./TestFiles/tc*.tig"
correctTestCases = glob.glob(correctTestCasesDir)
for correctTestCase in correctTestCases:
  fileName = correctTestCase.split('/')[-1]
  os.system("bash getAsm.sh " + fileName)
  if (fileName in inputAskingTestCases):
    os.system("java -jar /opt/RARS/rars1_3_1.jar sm nc " + fileName + ".s < " + correctTestCase + ".in > " + correctTestCase + ".c.out")
  else:
    os.system("java -jar /opt/RARS/rars1_3_1.jar sm nc " + fileName + ".s > " + correctTestCase + ".c.out")
  ok = True
  with open(correctTestCase + ".out", 'r') as origT:
    with open(correctTestCase + ".c.out", 'r') as genT:
        diff = difflib.unified_diff(
            origT.readlines(),
            genT.readlines(),
            fromfile = fileName + ".out",
            tofile = fileName + ".c.out",
        )
        for line in diff:
            ok = False
            sys.stdout.write(line)
  
  os.remove(fileName + ".s")
  os.remove(correctTestCase + ".c.out")
  assert ok == True

os.chdir(oldDir)
print("\n\n============All Passed============\n\n")