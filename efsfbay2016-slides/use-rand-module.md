footer: Kenji Rikitake / ErLounge 10-MAR-2016
slidenumbers: true

<!-- Use Deckset 1.6.0, Fira theme, 16:9 aspect ratio -->

# [fit] We Interrupt Your Regularly Scheduled Programming to Bring You
# [fit] A Public Service Announcement

---

# [fit] Use *rand* module
# now[^1]

[^1]: Unless you stick to the older Erlang/OTP or Elixir versions before Erlang/OTP 18.x

---

# [fit] The random module is
# [fit] obsolete
# [fit] and will be compromised by a brute-force attack in 9 hours![^2]

[^2]: <https://github.com/jj1bdx/as183-c>

---

# [fit] Still many examples use
# [fit] `random:uniform/1`
# [fit] This is not good

---

# [fit] Stop using
# [fit] *random* module
# now[^3]

[^3]: Unless you stick to the older Erlang/OTP or Elixir versions before Erlang/OTP 18.x

---

# [fit] Use `rand:uniform/1`
# [fit] or `:rand.uniform()`

---

# Alternatives

* [exsplus116](https://github.com/jj1bdx/exsplus116/), equivalent to rand module's default algorithm, for 17.x or older versions of Erlang/OTP
* [sfmt-erlang](https://github.com/jj1bdx/sfmt-erlang/) (hex.pm: [sfmt](https://hex.pm/packages/sfmt))
* [tinymt-erlang](https://github.com/jj1bdx/tinymt-erlang/) (hex.pm: [tinymt](https://hex.pm/packages/tinymt))

---

## I repeat:
# [fit] use *rand* module
## right now[^4]!

[^4]: Unless you stick to the older Erlang/OTP or Elixir versions before Erlang/OTP 18.x

---

## [fit] random module will be
#[fit] deprecated in OTP 19
#[fit] removed from OTP 20

(Officially announced by Kenneth Lundin on Erlang Factory SF Bay 2016 Day 1)

---

# [fit] Thank you
