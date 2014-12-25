Feature: Do Some things
  In order to use rake from Emacs
  As a user
  I want to interact with rake tool

  Scenario: Run rake
    Given I have Rakefile with content:
    """
    task :foo do
      puts 'foo'
    end
    """
    When I run command "rake-compile" selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "rake foo"
