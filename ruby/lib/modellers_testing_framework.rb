# modellers_testing_framework
#
# Author::    Peter Lane
# Copyright:: Copyright 2011, Peter Lane.
# License::   GPLv3
#
# modellers_testing_framework is free software: you can redistribute it 
# and/or modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation, either version 3 of the 
# License, or (at your option) any later version.
#
# modellers_testing_framework is distributed in the hope that it will 
# be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with modellers_testing_framework.  
# If not, see <http://www.gnu.org/licenses/>.

module TestFramework

  private
  # A test class for tests
  class TestException < Exception
  end

  public
  # Simple assertion method: 
  # raises an exception with given message if test is false.
  def assert(test, msg = "")
    raise TestException.new msg unless test
  end

  # Simple assertion method: 
  # raises an exception with given message if test is false.
  def assert_true(test, msg = "")
    raise TestException.new msg unless test
  end

  # Simple assertion method: 
  # raises an exception with given message if test is true.
  def assert_false(test, msg = "")
    raise TestException.new msg if test
  end

  # Assert equality of given two items:
  # raises an exception with given message if items are different.
  def assert_equal(expected, computed, msg = "")
    unless expected == computed
      raise TestException.new "Expected #{expected}, but calculated #{computed} #{msg == "" ? "" : "-"} #{msg}"
    end
  end

  # Assert that two items should not be equal:
  # raises an exception with given message if items are equal.
  def assert_not_equal(expected, computed, msg = "")
    if expected == computed
      raise TestException.new "Expected not equal to #{expected}, but calculated #{computed} #{msg == "" ? "" : "-"} #{msg}"
    end
  end

  private
  # General purpose method to run a list of results and 
  # display progress.
  def TestFramework.run_tests(test_list, test_name)
    # flag used to indicate if a newline needed when printing test
    last_was_error = false 

    error_count = 0
    print "#{test_name} tests: "
    test_list.each do |pair|
      begin
        pair[1].call
        print "."
        last_was_error = false 
      rescue TestException => te 
        puts "\n" unless last_was_error
        puts "Error in #{pair[0]}: #{te.to_s}"
        last_was_error = true
        error_count += 1
      end
    end
    puts "" unless last_was_error
    puts <<-END
There #{error_count == 1 ? "was" : "were"} \
#{error_count} error#{error_count == 1 ? "" : "s"} \
out of #{test_list.size} test#{test_list.size == 1 ? "" : "s"}.
END
  end

  private
  # Storage for Unit Tests
  TestFramework::UnitTests = []

  public
  # Define a unit test with optional name.
  # Assertions and other code should be placed within a block.
  def unit_test(name = "", &block)
    TestFramework::UnitTests << [name, block]
  end

  public
  # Run the unit tests alone
  def TestFramework.run_unit_tests
    TestFramework.run_tests(UnitTests, "Unit")
  end

  private
  # Storage for Process Tests
  TestFramework::ProcessTests = []

  public
  # Define a process test with optional name.
  # Assertions and other code should be placed within a block.
  def process_test(name = "", &block)
    TestFramework::ProcessTests << [name, block]
  end

  public
  # Run the process tests alone
  def TestFramework.run_process_tests
    TestFramework.run_tests(ProcessTests, "Process")
  end

  private
  # Storage for Canonical Result Tests
  TestFramework::CanonicalResultTests = []

  public
  # Define a canonical result test with optional name.
  # Assertions and other code should be placed within a block.
  def canonical_result_test(name = "", &block)
    TestFramework::CanonicalResultTests << [name, block]
  end

  public
  # Run the canonical result tests alone
  def TestFramework.run_canonical_result_tests
    TestFramework.run_tests(CanonicalResultTests, "Canonical Result")
  end

  public
  # Run all tests
  def TestFramework.run_all_tests
    TestFramework.run_unit_tests
    TestFramework.run_process_tests
    TestFramework.run_canonical_result_tests
  end
end

