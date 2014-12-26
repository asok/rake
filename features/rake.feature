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

  Scenario: Run rake
    When I run command "rake" selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "rake foo"

  Scenario: Run rake with bundler
    And I have Gemfile
    When I run command "rake" selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "bundle exec rake foo"

  Scenario: Run rake with zeus
    And zeus is running
    When I run command "rake" selecting "foo"
    And I switch to buffer "*rake-compilation*"
    Then I should see "zeus rake foo"
