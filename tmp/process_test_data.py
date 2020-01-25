tests = []

with open('test.data') as file_object:
    for line in file_object:
        tests.append(line.rstrip().split('"')[1])

tests = list(set(tests))
tests.sort()

# First filter out arith tests:
arith_tests = [test for test in tests if 'l ' not in test and '(' not in test and 'x' not in test]

for test in arith_tests:
    print(test)

print("---------")

# Next filter out lambda tests:
lambda_tests = [test for test in tests if 'l ' in test or '(' in test or 'x' in test]

for test in lambda_tests:
    print(test)


print("---------")
print(f'num all tests: {len(tests)},'
      f' num arith tests: {len(arith_tests)},'
      f' num lambda tests: {len(lambda_tests)},'
      f' arith + lambda tests: {len(arith_tests) + len(lambda_tests)}')
