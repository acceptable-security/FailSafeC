#include "common.h"
#include <netinet/in.h>

TEST_CASE_S(in6_is_addr_short_buffer, FSC_ABRT)
{
  char buf[15];
  IN6_IS_ADDR_UNSPECIFIED((struct in6_addr *)&buf);
}

static struct in6_addr unspec = {{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr loopback = {{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 }};
static struct in6_addr linklocal = {{ 0xfe, 0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr sitelocal = {{ 0xfe, 0xc0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr v4compat = {{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 127, 0, 0, 1 }};
static struct in6_addr v4mapped = {{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 127, 0, 0, 1 }};
static struct in6_addr mc_nodelocal = {{ 0xff, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr mc_linklocal = {{ 0xff, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr mc_sitelocal = {{ 0xff, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};
static struct in6_addr mc_global    = {{ 0xff, 0xe, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }};

TEST_CASE(in6_is_addr_unspecified)
{
  TEST(
     IN6_IS_ADDR_UNSPECIFIED(&unspec) &&
    !IN6_IS_ADDR_UNSPECIFIED(&loopback) &&
    !IN6_IS_ADDR_UNSPECIFIED(&linklocal) &&
    !IN6_IS_ADDR_UNSPECIFIED(&sitelocal) &&
    !IN6_IS_ADDR_UNSPECIFIED(&v4compat) &&
    !IN6_IS_ADDR_UNSPECIFIED(&v4mapped) &&
    !IN6_IS_ADDR_UNSPECIFIED(&mc_nodelocal) &&
    !IN6_IS_ADDR_UNSPECIFIED(&mc_linklocal) &&
    !IN6_IS_ADDR_UNSPECIFIED(&mc_sitelocal) &&
    !IN6_IS_ADDR_UNSPECIFIED(&mc_global)
    );
}

TEST_CASE(in6_is_addr_loopback)
{
  TEST(
    !IN6_IS_ADDR_LOOPBACK(&unspec) &&
     IN6_IS_ADDR_LOOPBACK(&loopback) &&
    !IN6_IS_ADDR_LOOPBACK(&linklocal) &&
    !IN6_IS_ADDR_LOOPBACK(&sitelocal) &&
    !IN6_IS_ADDR_LOOPBACK(&v4compat) &&
    !IN6_IS_ADDR_LOOPBACK(&v4mapped) &&
    !IN6_IS_ADDR_LOOPBACK(&mc_nodelocal) &&
    !IN6_IS_ADDR_LOOPBACK(&mc_linklocal) &&
    !IN6_IS_ADDR_LOOPBACK(&mc_sitelocal) &&
    !IN6_IS_ADDR_LOOPBACK(&mc_global)
    );
}

TEST_CASE(in6_is_addr_linklocal)
{
  TEST(
    !IN6_IS_ADDR_LINKLOCAL(&unspec) &&
    !IN6_IS_ADDR_LINKLOCAL(&loopback) &&
     IN6_IS_ADDR_LINKLOCAL(&linklocal) &&
    !IN6_IS_ADDR_LINKLOCAL(&sitelocal) &&
    !IN6_IS_ADDR_LINKLOCAL(&v4compat) &&
    !IN6_IS_ADDR_LINKLOCAL(&v4mapped) &&
    !IN6_IS_ADDR_LINKLOCAL(&mc_nodelocal) &&
    !IN6_IS_ADDR_LINKLOCAL(&mc_linklocal) &&
    !IN6_IS_ADDR_LINKLOCAL(&mc_sitelocal) &&
    !IN6_IS_ADDR_LINKLOCAL(&mc_global)
    );
}

TEST_CASE(in6_is_addr_sitelocal)
{
  TEST(
    !IN6_IS_ADDR_SITELOCAL(&unspec) &&
    !IN6_IS_ADDR_SITELOCAL(&loopback) &&
    !IN6_IS_ADDR_SITELOCAL(&linklocal) &&
     IN6_IS_ADDR_SITELOCAL(&sitelocal) &&
    !IN6_IS_ADDR_SITELOCAL(&v4compat) &&
    !IN6_IS_ADDR_SITELOCAL(&v4mapped) &&
    !IN6_IS_ADDR_SITELOCAL(&mc_nodelocal) &&
    !IN6_IS_ADDR_SITELOCAL(&mc_linklocal) &&
    !IN6_IS_ADDR_SITELOCAL(&mc_sitelocal) &&
    !IN6_IS_ADDR_SITELOCAL(&mc_global)
    );
}

TEST_CASE(in6_is_addr_v4mapped)
{
  TEST(
    !IN6_IS_ADDR_V4MAPPED(&unspec) &&
    !IN6_IS_ADDR_V4MAPPED(&loopback) &&
    !IN6_IS_ADDR_V4MAPPED(&linklocal) &&
    !IN6_IS_ADDR_V4MAPPED(&sitelocal) &&
    !IN6_IS_ADDR_V4MAPPED(&v4compat) &&
     IN6_IS_ADDR_V4MAPPED(&v4mapped) &&
    !IN6_IS_ADDR_V4MAPPED(&mc_nodelocal) &&
    !IN6_IS_ADDR_V4MAPPED(&mc_linklocal) &&
    !IN6_IS_ADDR_V4MAPPED(&mc_sitelocal) &&
    !IN6_IS_ADDR_V4MAPPED(&mc_global)
    );
}

TEST_CASE(in6_is_addr_v4compat)
{
  TEST(
    !IN6_IS_ADDR_V4COMPAT(&unspec) &&
    !IN6_IS_ADDR_V4COMPAT(&loopback) &&
    !IN6_IS_ADDR_V4COMPAT(&linklocal) &&
    !IN6_IS_ADDR_V4COMPAT(&sitelocal) &&
     IN6_IS_ADDR_V4COMPAT(&v4compat) &&
    !IN6_IS_ADDR_V4COMPAT(&v4mapped) &&
    !IN6_IS_ADDR_V4COMPAT(&mc_nodelocal) &&
    !IN6_IS_ADDR_V4COMPAT(&mc_linklocal) &&
    !IN6_IS_ADDR_V4COMPAT(&mc_sitelocal) &&
    !IN6_IS_ADDR_V4COMPAT(&mc_global)
    );
}

TEST_CASE(in6_is_addr_mc_nodelocal)
{
  TEST(
    !IN6_IS_ADDR_MC_NODELOCAL(&unspec) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&loopback) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&linklocal) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&sitelocal) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&v4compat) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&v4mapped) &&
     IN6_IS_ADDR_MC_NODELOCAL(&mc_nodelocal) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&mc_linklocal) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&mc_sitelocal) &&
    !IN6_IS_ADDR_MC_NODELOCAL(&mc_global)
    );
}

TEST_CASE(in6_is_addr_mc_linklocal)
{
  TEST(
    !IN6_IS_ADDR_MC_LINKLOCAL(&unspec) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&loopback) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&linklocal) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&sitelocal) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&v4compat) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&v4mapped) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&mc_nodelocal) &&
     IN6_IS_ADDR_MC_LINKLOCAL(&mc_linklocal) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&mc_sitelocal) &&
    !IN6_IS_ADDR_MC_LINKLOCAL(&mc_global)
    );
}

TEST_CASE(in6_is_addr_mc_sitelocal)
{
  TEST(
    !IN6_IS_ADDR_MC_SITELOCAL(&unspec) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&loopback) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&linklocal) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&sitelocal) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&v4compat) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&v4mapped) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&mc_nodelocal) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&mc_linklocal) &&
     IN6_IS_ADDR_MC_SITELOCAL(&mc_sitelocal) &&
    !IN6_IS_ADDR_MC_SITELOCAL(&mc_global)
    );
}

TEST_CASE(in6_is_addr_mc_global)
{
  TEST(
    !IN6_IS_ADDR_MC_GLOBAL(&unspec) &&
    !IN6_IS_ADDR_MC_GLOBAL(&loopback) &&
    !IN6_IS_ADDR_MC_GLOBAL(&linklocal) &&
    !IN6_IS_ADDR_MC_GLOBAL(&sitelocal) &&
    !IN6_IS_ADDR_MC_GLOBAL(&v4compat) &&
    !IN6_IS_ADDR_MC_GLOBAL(&v4mapped) &&
    !IN6_IS_ADDR_MC_GLOBAL(&mc_nodelocal) &&
    !IN6_IS_ADDR_MC_GLOBAL(&mc_linklocal) &&
    !IN6_IS_ADDR_MC_GLOBAL(&mc_sitelocal) &&
     IN6_IS_ADDR_MC_GLOBAL(&mc_global)
    );
}
