## Snaplet-Environments

Snap Framework ( http://snapframework.com ) support for having different
configuration environments.

Ex. you can have development database "your_app_dev" and production
"your_app_prod" and test "your_app_test" and switch between them
just by running your app in those environments.

Depends on Snap 0.7.* and others

### Overview

This isn't traditional snaplet that adds something to your App state.
It just base its works on Snap's configuration API giving you a more
easy to use ways of getting conf values for environment that your app
is currently running.

Let's say you have defined "development" environment in your _snaplet.cfg_
Then running app in this env is as simple as:

```
yourapp @development
```

### Defining environments

You just have to put your environments group in config file like ex.:

```
# Your App main configuration file

database-prefix = "yourapp"
database-address = "localhost"

recaptcha-test-mode = false
recaptcha-key  = "sdfgsfdgsdfgsdgs"

heist-root = "resources/templates"

mailers-test-mode = false

environments
{
  development
  {
    database-name = "$(database-prefix)_development"
  }

  production
  {
    database-name = "$(database-prefix)_production"
  }

  test
  {
    database-name = "$(database-prefix)_test"
    recaptcha-test-mode = true
    mailers-test-mode = true
  }
}
```

If you don't specify which env to run your app - it'll choose first 
env declaration for you. In this exaple:

```
yourapp
```

Would run yourapp in "development" environment.

### Integration

In Site.hs

```
-- (...)
makeSnaplet "app" "An snaplet example application." Nothing $ do
    heistRoot   <- lookupEnvDefault "heist-root" "resources/templates"

    dbName      <- lookupEnvDefault "database-name"        "yourapp_development"
    dbAddress   <- lookupEnvDefault "database-address"     "localhost"
    dbConns     <- lookupEnvDefault "database-connections" 12

    h  <- nestSnaplet "heist" heist $ do
            heistInit heistRoot
    m  <- nestSnaplet "mongoDB" mongoDB $
            mongoDBInit (host dbAddress) dbConns dbName

    --- (...)
```

The "lookupEnvDefault" helper function reads value from current environment group 
or if it doesn't have given key - from root of conf file.

```
lookupEnvDefault :: (Configured a, Monad (m b v), MonadSnaplet m, MonadIO (m b v)) => Name -> a -> m b v a
```

That's all:)