import sys

# memory_initialization_radix = 16;
# memory_initialization_vector =
# f0000513,
# 00a00293,
# 00552023,
# fe000ae3;

if __name__ == "__main__":
    '''
    input xxx.txt in instruction code
    output xxx.coe
    '''
    if len(sys.argv)!= 2:
        print(f'Invalid args number:{len(sys.argv)}')
        print(f'args: {sys.argv}')
        sys.exit(1)
    filename = sys.argv[1]
    with open(filename,'r') as file:
        lines = file.readlines()
        with open(filename.replace('.txt','.coe'),'w') as coe:
            coe.write('memory_initialization_radix = 16;\n')
            coe.write('memory_initialization_vector =\n')
            last_line = len(lines)-1
            for i,line in enumerate(lines):
                if i == last_line:
                    coe.write(f'{line.strip()};\n')
                else:
                    coe.write(f'{line.strip()},\n')
