library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.

# 3. Get OAuth credentials
myapp <- oauth_app("github",
                   key = "0ab73f323f51cc40ce22",
                   secret = "5c2518b67f7e6063d3172559a6230a5838c7aca9"
)


github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)


# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
 content(req)

# OR:
# req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
# stop_for_status(req)
# content(req)