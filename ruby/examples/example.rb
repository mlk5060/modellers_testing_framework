require "modellers_testing_framework"

include TestFramework

unit_test "first" do
  assert(true, "first test")
end

unit_test "second" do
  assert(false, "second test")
end

class A
  include TestFramework

  def double n
    3 * n
  end

  unit_test "double" do
    a = A.new
    assert_equal(4, a.double(2))
  end
end

process_test "first" do
  assert_true(true, "first test")
end

canonical_result_test "first" do
  assert_true(true, "first test")
end

process_test "second" do
  assert_false(false, "second test")
end

TestFramework.run_all_tests
