Feature: Do Some things
  In order to use rake from Emacs
  As a user
  I want to interact with rake tool

  Background:
    Given I have Rakefile with content:
    """
    task :foo do
      puts 'foo'
    end
    """

  Scenario: Running rake without cache
    When I run rake selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "rake foo"
    And the task "foo" is not in the cache

  Scenario: Running rake with cache
    Given I enable the cache
    When I run rake selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "rake foo"
    And the task "foo  # " is in the cache

  Scenario: Running rake with an argument
    When I run rake selecting "foo" and setting "bar" as an argument
    And I switch to buffer "*rake-compilation*"
    Then I should see "rake foo bar"

  Scenario: Running rake with bundler
    And I have Gemfile
    When I run rake selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "bundle exec rake foo"

  Scenario: Running rake with zeus
    And zeus is running
    When I run rake selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "zeus rake foo"

  Scenario: Running rake with spring
    And spring is running
    When I run rake selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "spring rake foo"
