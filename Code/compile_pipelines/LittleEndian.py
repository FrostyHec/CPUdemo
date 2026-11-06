import sys

if __name__ == "__main__":
    '''
    input xxx.txt in instruction code in big endian
    output xxx.txt  in little endian
    '''
    if len(sys.argv)!= 2:
        print(f'Invalid args number:{len(sys.argv)}')
        sys.exit(1)
    filename = sys.argv[1]
    with open(filename,'r') as file:
        lines = file.readlines()
        with open(filename.replace('.txt','_out.txt'),'w') as out:
            out.write("13000000" + '\n')
            for line in lines:
                line = line.strip()
                if line == '':
                    continue
                line = line[6:8] + line[4:6] + line[2:4] + line[0:2]
                out.write(line + '\n')