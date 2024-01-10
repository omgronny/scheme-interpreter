import os

def test(name, input_file, output_file, method):
    stream = os.popen(f"cat test/{input_file} | ./interpolate  --step=0.5 --window=3 --method={method}")
    res = stream.readlines()
    file = open(f"test/{output_file}", "r")
    excepted = file.readlines()

    for i in range(len(excepted)):
        assert res[i].split(":")[-1] == excepted[i].split(":")[-1]

    print(f"{name} passed")

test("test1 linear", "data/test1", "data/test1_output", "linear")
test("test2 linear", "data/test2", "data/test2_output_lin", "linear")

test("test1 lagrangia", "data/test1", "data/test1_output_lagr", "lagrangia")
test("test2 lagrangia", "data/test2", "data/test2_output_lagr", "lagrangia")
