import os
import pypandoc
import sys
import glob
from PyPDF2 import PdfFileMerger

inputAskingTestCases = set(["tc3.tig", "tc5.tig", "tc6.tig", "tc8.tig"])

correctTestCasesDir = "./TestFiles/tc*.tig"
correctTestCases = glob.glob(correctTestCasesDir)
for correctTestCase in correctTestCases:
  fileName = correctTestCase.split('/')[-1]
  os.system("bash getAsm.sh " + fileName)
  if (fileName in inputAskingTestCases):
    os.system("java -jar /opt/RARS/rars1_3_1.jar sm nc " + fileName + ".s < " + correctTestCase + ".in > " + correctTestCase + ".out")
  else:
    os.system("java -jar /opt/RARS/rars1_3_1.jar sm nc " + fileName + ".s > " + correctTestCase + ".out")
  os.remove(fileName + ".s")
