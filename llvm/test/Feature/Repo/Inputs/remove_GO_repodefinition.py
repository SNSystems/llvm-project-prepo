import argparse
import re

def filter_definition (line, GOList):
  filterStr = ",? !repo_definition ![0-9]+"
  for i in GOList:
    matchedFunction = re.compile(r'define .* @{0}.*'.format(i) + filterStr)
    matchedVariable = re.compile(r'@{0} = .*'.format(i) + filterStr)
    if (matchedFunction.search(line) or matchedVariable.search(line)):
      return re.sub(filterStr, '', line)
  return line

def remove_GO_ticketnode (GOList, inputFile, outputFile):
  """
  Remove the GO's repo_ticket metadata.
  """
  with open(inputFile, 'r') as f:
    lines = f.readlines()
  newLines = map(lambda x:filter_definition(x, GOList),lines)
  with open(outputFile, 'w') as f:
    f.writelines(newLines)

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('-r','--global-object-list', default=[], nargs='+', help='A global object list whose ticketnode need to be removed.')
  parser.add_argument('-o','--output-file', default="out.ll", help='the output file name.')
  requiredArguments = parser.add_argument_group('required arguments')
  requiredArguments.add_argument('-i','--input-file', help='the input file name.', required=True)
  args = parser.parse_args()

  remove_GO_ticketnode(args.global_object_list, args.input_file, args.output_file)

if __name__ == '__main__':
  main()
