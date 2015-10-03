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
      raise TestException.new "Expected #{expected.to_s}, but calculated #{computed.to_s} #{msg == "" ? "" : "-"} #{msg}"
    end
  end

  # Assert that two items should not be equal:
  # raises an exception with given message if items are equal.
  def assert_not_equal(expected, computed, msg = "")
    if expected == computed
      raise TestException.new "Expected not equal to #{expected.to_s}, but calculated #{computed.to_s} #{msg == "" ? "" : "-"} #{msg}"
    end
  end

  private
  # General purpose method to run a list of results and 
  # display progress.
  def TestFramework.run_tests(test_list, test_name)
    # flag used to indicate if a newline needed when printing test
    last_was_error = false 

    error_count = 0
    previous_test_suite = ""

    # Print out type of tests in blue.
    puts "\n\e[34m===== #{test_name} tests =====\e[0m"
    print_file_name = true
    test_list.each do |pair|
      begin

        #Grab the procedure run (pair[1]), convert it to a string, get everything from "@"" to ":"", 
        #chop off ":" (chop) and "@" ([1..-1]).  This gives the name of the file where tests are being
        #run
        test_suite = pair[1].to_s[/@(...)(.)*\:/].chop[1..-1] 
        if(test_suite != previous_test_suite)
          puts "\n" + test_suite
          previous_test_suite = test_suite
        end

        print "   #{pair[0]}: "
        pair[1].call
        last_was_error = false 

        # Print "OK" in green
        print "\e[32mOK\e[0m\n"
      rescue TestException => te 
        puts "" unless last_was_error

        # Print error message in red.
        print "\e[31m ERROR: #{te.to_s} \e[0m\n"
        last_was_error = true
        error_count += 1
      end
    end
    puts "" unless last_was_error

    # Summary in yellow
    puts <<-END
\e[33mThere #{error_count == 1 ? "was" : "were"} \
#{error_count} error#{error_count == 1 ? "" : "s"} \
out of #{test_list.size} test#{test_list.size == 1 ? "" : "s"}.\e[0m
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

