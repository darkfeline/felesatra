package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

type module struct {
	name string
	repo string
}

// Also update goserver.go
var modules = []module{
	{
		name: "go.felesatra.moe/anaclock",
		repo: "https://github.com/darkfeline/anaclock",
	},
	{
		name: "go.felesatra.moe/anidb",
		repo: "https://github.com/darkfeline/anidb-go",
	},
	{
		name: "go.felesatra.moe/animanager",
		repo: "https://github.com/darkfeline/animanager-go",
	},
	{
		name: "go.felesatra.moe/binpack",
		repo: "https://github.com/darkfeline/binpack",
	},
	{
		name: "go.felesatra.moe/cloudflare",
		repo: "https://github.com/darkfeline/cloudflare-go",
	},
	{
		name: "go.felesatra.moe/danbooru",
		repo: "https://github.com/darkfeline/danbooru-go",
	},
	{
		name: "go.felesatra.moe/database/sql/sqlite3/migrate",
		repo: "https://github.com/darkfeline/go-sqlite3-migrate",
	},
	{
		name: "go.felesatra.moe/dlsite",
		repo: "https://github.com/darkfeline/dlsite-go",
	},
	{
		name: "go.felesatra.moe/dlsite/v2",
		repo: "https://github.com/darkfeline/dlsite-go",
	},
	{
		name: "go.felesatra.moe/encoding/sexp",
		repo: "https://github.com/darkfeline/go-sexp",
	},
	{
		name: "go.felesatra.moe/go2/errors",
		repo: "https://github.com/darkfeline/go2-errors",
	},
	{
		name: "go.felesatra.moe/hash/ed2k",
		repo: "https://github.com/darkfeline/ed2k-go",
	},
	{
		name: "go.felesatra.moe/keeper",
		repo: "https://github.com/darkfeline/keeper-go",
	},
	{
		name: "go.felesatra.moe/linelist",
		repo: "https://github.com/darkfeline/go-linelist",
	},
	{
		name: "go.felesatra.moe/pwnck",
		repo: "https://github.com/darkfeline/pwnck",
	},
	{
		name: "go.felesatra.moe/qualia",
		repo: "https://github.com/darkfeline/qualia-go",
	},
	{
		name: "go.felesatra.moe/saucenao",
		repo: "https://github.com/darkfeline/saucenao",
	},
	{
		name: "go.felesatra.moe/sitemap",
		repo: "https://github.com/darkfeline/sitemap-go",
	},
	{
		name: "go.felesatra.moe/xdg",
		repo: "https://github.com/darkfeline/xdg-go",
	},
}

func main() {
	verbose := flag.Bool("verbose", false, "Be verbose")
	flag.Parse()
	if *verbose {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}
	workDir := flag.Arg(0)
	outDir := flag.Arg(1)
	if workDir == "" || outDir == "" {
		printUsage(os.Stderr)
		log.Fatal("Missing arguments")
	}

	if err := os.MkdirAll(workDir, 0o777); err != nil {
		log.Fatal(err)
	}
	if err := os.MkdirAll(outDir, 0o777); err != nil {
		log.Fatal(err)
	}

	for _, m := range modules {
		if err := buildModule(workDir, outDir, m); err != nil {
			log.Fatal(err)
		}
	}
}

func printUsage(w io.Writer) {
	fmt.Fprintf(w, "Usage: %v WORKDIR OUTDIR\n", os.Args[0])
}

func buildModule(workDir, outDir string, m module) error {
	l := slog.With("module", m.name)
	l.Debug("Building module")
	r, err := newRepo(m, workDir)
	if err != nil {
		return fmt.Errorf("build module %q: %s", m.name, err)
	}

	versions, err := r.versions()
	if err != nil {
		return fmt.Errorf("build module %q: %s", m.name, err)
	}
	l.Debug("Got versions for module", "versions", versions)

	mt, err := newModuleTree(outDir, m)
	if err != nil {
		return fmt.Errorf("build module %q: %s", m.name, err)
	}

	var good []version
	for _, v := range versions {
		l := l.With("version", v)
		if _, err := r.getModFile(v); err != nil {
			l.Warn("Module version missing go.mod; skipping")
			l.Debug("Error getting mod file", "error", err)
			continue
		}
		l.Debug("Building module version")
		if err := buildModuleVersion(l, mt, r, v); err != nil {
			return fmt.Errorf("build module %q version %q: %s", m.name, v, err)
		}
		good = append(good, v)
	}
	if err := writeModuleVersionList(mt, good); err != nil {
		return fmt.Errorf("build module %q: %s", m.name, err)
	}
	return nil
}

func buildModuleVersion(l *slog.Logger, mt moduleTree, r repo, v version) error {
	l.Debug("Writing info file")
	if err := writeInfoFile(mt, r, v); err != nil {
		return err
	}
	l.Debug("Writing mod file")
	if err := writeModFile(mt, r, v); err != nil {
		return err
	}
	l.Debug("Writing archive")
	if err := writeModArchive(mt, r, v); err != nil {
		return err
	}
	return nil
}

func writeInfoFile(mt moduleTree, r repo, v version) error {
	mi, err := r.modInfo(v)
	if err != nil {
		return fmt.Errorf("write info file: %s", err)
	}
	b, err := json.Marshal(mi)
	if err != nil {
		return fmt.Errorf("write info file: %s", err)
	}
	if err := ioutil.WriteFile(mt.infoFile(v), b, 0o666); err != nil {
		return fmt.Errorf("write info file: %s", err)
	}
	return nil
}

func writeModFile(mt moduleTree, r repo, v version) error {
	b, err := r.getModFile(v)
	if err != nil {
		return fmt.Errorf("write mod file: %s", err)
	}
	if err := ioutil.WriteFile(mt.modFile(v), b, 0o666); err != nil {
		return fmt.Errorf("write mod file: %s", err)
	}
	return nil
}

func writeModArchive(mt moduleTree, r repo, v version) error {
	b, err := r.getRepoZip(v)
	if err != nil {
		return fmt.Errorf("write mod archive: %s", err)
	}
	if err := ioutil.WriteFile(mt.zipFile(v), b, 0o666); err != nil {
		return fmt.Errorf("write mod archive: %s", err)
	}
	return nil
}

func writeModuleVersionList(mt moduleTree, v []version) error {
	var b bytes.Buffer
	for _, v := range v {
		b.Write([]byte(v))
		b.Write([]byte("\n"))
	}
	err := ioutil.WriteFile(mt.versionListFile(), b.Bytes(), 0o666)
	if err != nil {
		return fmt.Errorf("write module version list: %s", err)
	}
	return nil
}

type repo struct {
	module
	checkout string
}

func newRepo(m module, workDir string) (repo, error) {
	checkout := filepath.Join(workDir, m.name)
	ok, err := exists(checkout)
	if err != nil {
		return repo{}, fmt.Errorf("new repo %q: %s", checkout, err)
	}
	if ok {
		slog.Debug("Fetching repo", "module", m.name)
		c := exec.Command("git", "-C", checkout, "fetch", "-t", "origin")
		if err := c.Run(); err != nil {
			return repo{}, fmt.Errorf("new repo %q: fetch failed: %s", checkout, err)
		}
	} else {
		slog.Debug("Cloning repo", "module", m.name)
		if err := os.MkdirAll(checkout, 0o777); err != nil {
			return repo{}, fmt.Errorf("build module %v: %s", m.name, err)
		}
		c := exec.Command("git", "clone", "--bare", m.repo, checkout)
		if err := c.Run(); err != nil {
			return repo{}, fmt.Errorf("new repo %q: clone failed: %s", checkout, err)
		}
	}
	return repo{
		module:   m,
		checkout: checkout,
	}, nil
}

func (r repo) versions() (_ []version, _ error) {
	c := exec.Command("git", "-C", r.checkout, "tag")
	out, err := c.Output()
	if err != nil {
		return nil, fmt.Errorf("get versions in %q: %s", r.checkout, err)
	}
	var vs []version
	for _, s := range strings.Split(string(out), "\n") {
		if isVersionTagFor(s, r.module) {
			vs = append(vs, version(s))
		}
	}
	return vs, nil
}

func (r repo) getModFile(v version) ([]byte, error) {
	c := exec.Command("git", "-C", r.checkout, "show", fmt.Sprintf("%s:go.mod", v))
	return c.Output()
}

func (r repo) getRepoZip(v version) ([]byte, error) {
	c := exec.Command("git", "-C", r.checkout, "archive", "--format=zip",
		fmt.Sprintf("--prefix=%s@%s/", r.module.name, v),
		string(v))
	return c.Output()
}

type version string

func (r repo) modInfo(v version) (modInfo, error) {
	t, err := r.commitTime(v)
	if err != nil {
		return modInfo{}, fmt.Errorf("get module info for %q in %q: %s",
			v, r.checkout, err)
	}
	return modInfo{
		Version: v,
		Time:    t,
	}, nil
}

type modInfo struct {
	Version version `json:"version"`
	Time    string  `json:"time"`
}

func (r repo) commitTime(v version) (string, error) {
	c := exec.Command("git", "-C", r.checkout, "show", "-s", "--format=%cI",
		string(v))
	out, err := c.Output()
	if err != nil {
		return "", fmt.Errorf("get commit time for %q in %q: %s",
			v, r.checkout, err)
	}
	return strings.TrimSpace(string(out)), nil
}

var (
	verPat = regexp.MustCompile(`/(v[0-9]+)$`)
	v1Pat  = regexp.MustCompile(`v[01]\.`)
)

func isVersionTagFor(tag string, m module) bool {
	v := verPat.FindString(m.name)
	if v == "" {
		return v1Pat.MatchString(tag)
	}
	return strings.HasPrefix(tag, v+".")
}

type moduleTree struct {
	path string
}

func newModuleTree(outDir string, m module) (moduleTree, error) {
	p := filepath.Join(outDir, m.name, "@v")
	if err := os.MkdirAll(p, 0o777); err != nil {
		return moduleTree{}, fmt.Errorf("new module tree %q: %s", m, err)
	}
	return moduleTree{path: p}, nil
}

func (m moduleTree) versionListFile() string {
	return filepath.Join(m.path, "list")
}

func (m moduleTree) infoFile(v version) string {
	return filepath.Join(m.path, fmt.Sprintf("%s.info", v))
}

func (m moduleTree) modFile(v version) string {
	return filepath.Join(m.path, fmt.Sprintf("%s.mod", v))
}

func (m moduleTree) zipFile(v version) string {
	return filepath.Join(m.path, fmt.Sprintf("%s.zip", v))
}

func exists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return false, nil
		}
		return false, err
	}
	return true, nil
}
