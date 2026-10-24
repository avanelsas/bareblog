(ns bareblog.command.update-test
  "BareBlog brought to its latest release. The repository of BareBlog is a bare repository with
   two releases, and BareBlog is a clone of it, made anew for each test."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.command.update :as update]
            [bareblog.example :as example]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest version-test
  (testing "a version is three numbers, with or without a v before it"
    (is (= [0 3 0] (update/version "0.3.0")))
    (is (= [10 0 12] (update/version "v10.0.12"))))
  (testing "anything else names no version"
    (is (every? nil? (map update/version ["0.3" "v0.3.0-rc1" "main" "" nil])))))

(deftest latest-test
  (testing "the latest release is the newest version, by its numbers and not as text"
    (is (= [0 10 0] (update/latest ["v0.2.0" "v0.10.0" "v0.9.1"]))))
  (testing "a tag with no v, or that names no version, is no release"
    (is (= [0 2 0] (update/latest ["v0.2.0" "1.0.0" "vnext"])))
    (is (nil? (update/latest ["1.0.0" "draft"])))
    (is (nil? (update/latest [])))))

(def changelog
  "# Changelog\n\nIntro.\n\n## [Unreleased]\n\nNext.\n\n## [0.3.0] - 2026-10-24\n\nThird.\n\n## [0.2.0] - 2026-10-23\n\nSecond.\n\n## [0.1.0] - 2026-10-20\n\nFirst.\n")

(deftest changes-test
  (testing "the changes are the sections after the installed version, up to and with the latest"
    (is (= "## [0.3.0] - 2026-10-24\n\nThird.\n\n## [0.2.0] - 2026-10-23\n\nSecond."
           (update/changes changelog [0 1 0] [0 3 0])))
    (is (= "## [0.2.0] - 2026-10-23\n\nSecond." (update/changes changelog [0 1 0] [0 2 0]))))
  (testing "there are none from the latest version, or in a changelog with no sections"
    (is (= "" (update/changes changelog [0 3 0] [0 3 0])))
    (is (= "" (update/changes "# Changelog\n" [0 1 0] [0 3 0])))))

(def fit
  "What git says of a clone that may be updated."
  {:clone? true :changed? false :branch "main" :release? false})

(deftest refusal-test
  (testing "a clone with no changes of its own, on main or on no branch at a release, may be updated"
    (is (nil? (update/refusal "/b" fit)))
    (is (nil? (update/refusal "/b" (assoc fit :branch nil :release? true)))))
  (testing "BareBlog that is no clone, that has changes of its own, or that is elsewhere, may not"
    (is (re-find #"not a clone" (update/refusal "/b" (assoc fit :clone? false))))
    (is (re-find #"changes of its own" (update/refusal "/b" (assoc fit :changed? true))))
    (is (re-find #"on a-feature\. Switch to main" (update/refusal "/b" (assoc fit :branch "a-feature"))))
    (is (re-find #"on a-feature\." (update/refusal "/b" (assoc fit :branch "a-feature" :release? true))))
    (is (re-find #"no release" (update/refusal "/b" (assoc fit :branch nil))))))

(deftest plan-test
  (let [tags ["v0.1.0" "v0.2.0"]]
    (testing "an older version is updated to the latest release"
      (is (= {:to [0 2 0]} (update/plan "/b" [0 1 0] fit tags))))
    (testing "the latest version, or a newer one, is current"
      (is (= {:current [0 2 0]} (update/plan "/b" [0 2 0] fit tags)))
      (is (= {:current [0 3 0]} (update/plan "/b" [0 3 0] fit tags)))
      (is (= {:current [0 1 0]} (update/plan "/b" [0 1 0] fit []))))
    (testing "a repository that could not be reached is said"
      (is (= {:unreachable? true} (update/plan "/b" [0 1 0] fit nil))))
    (testing "a refusal comes before anything else"
      (is (re-find #"changes of its own"
                   (:refused (update/plan "/b" [0 1 0] (assoc fit :changed? true) nil)))))))

(defn- sh
  "What git says to `args` in `dir`, trimmed. Throws when git could not."
  [dir & args]
  (str/trim (:out (apply process/shell {:dir (str dir) :out :string :err :string} "git" args))))

(defn- release!
  "Commits BareBlog in `dir` at the version `v`, with the changelog up to it, and tags it."
  [dir v sections]
  (spit (fs/file dir "package.json") (str "{\"name\": \"bareblog\", \"version\": \"" v "\"}\n"))
  (spit (fs/file dir "CHANGELOG.md") (str "# Changelog\n\n" sections))
  (sh dir "add" "--all")
  (sh dir "commit" "--quiet" "-m" (str "Version " v))
  (sh dir "tag" (str "v" v)))

(defn- with-clone
  "What `check` gives for the directory of a clone of BareBlog at the release 0.1.0, whose
   repository has the releases 0.1.0 and 0.2.0, and the directory of that repository."
  [check]
  (example/with-empty-dir
    (fn [top]
      (let [made   (str (fs/create-dirs (fs/path top "made")))
            remote (str (fs/path top "bareblog.git"))
            home   (str (fs/path top "bareblog"))]
        (sh made "init" "--quiet" "-b" "main")
        (doseq [[k v] example/git-settings]
          (sh made "config" k v))
        (release! made "0.1.0" "## [0.1.0] - 2026-10-20\n\nFirst.\n")
        (release! made "0.2.0" "## [0.2.0] - 2026-10-23\n\nSecond.\n\n## [0.1.0] - 2026-10-20\n\nFirst.\n")
        (sh top "clone" "--quiet" "--bare" made remote)
        (sh remote "config" "gc.auto" "0")
        (sh top "clone" "--quiet" remote home)
        (sh home "config" "gc.auto" "0")
        (sh home "-c" "advice.detachedHead=false" "checkout" "--quiet" "v0.1.0")
        (check home remote)))))

(def ^:private run example/run-command)

(deftest version!-test
  (with-clone
    (fn [home _remote]
      (testing "the version is the one in package.json"
        (is (= {:exit 0 :said ["BareBlog 0.1.0"]} (run update/version! home nil)))))))

(deftest update!-no-test
  (with-clone
    (fn [home _remote]
      (testing "a newer release is named, with its changes, and a no changes nothing"
        (let [{:keys [exit said]} (run update/update! home false)]
          (is (zero? exit))
          (is (= "BareBlog 0.1.0 is installed. The latest release is 0.2.0." (first said)))
          (is (some #{"## [0.2.0] - 2026-10-23\n\nSecond."} said))
          (is (= "Nothing was changed." (last said)))
          (is (= "v0.1.0" (sh home "describe" "--tags"))))))))

(deftest update!-step-test
  (with-clone
    (fn [home _remote]
      (testing "a step that fails is said with what to run by hand, and the steps before it stay
                done. The clone has no package-lock.json, so npm ci fails."
        (let [{:keys [exit said]} (run update/update! home true)]
          (is (= 1 exit))
          (is (some #{"Installing what it needs ..."} said))
          (is (= (str "That failed. Run it in " home " yourself: npm ci") (last said)))
          (is (= "v0.2.0" (sh home "describe" "--tags"))))))))

(deftest update!-changed-test
  (with-clone
    (fn [home _remote]
      (testing "BareBlog with changes of its own is not updated"
        (spit (fs/file home "package.json") "{\"version\": \"0.1.0\", \"changed\": true}\n")
        (let [{:keys [exit said]} (run update/update! home false)]
          (is (= 1 exit))
          (is (re-find #"changes of its own" (first said))))))))

(deftest update!-branch-test
  (with-clone
    (fn [home _remote]
      (testing "BareBlog on a branch of its own is not updated, also at a release"
        (sh home "switch" "--quiet" "-c" "a-feature")
        (is (re-find #"on a-feature\." (first (:said (run update/update! home false)))))))))

(deftest update!-current-test
  (with-clone
    (fn [home _remote]
      (testing "BareBlog at the latest release says so"
        (sh home "-c" "advice.detachedHead=false" "checkout" "--quiet" "v0.2.0")
        (is (= {:exit 0 :said ["BareBlog 0.2.0 is the latest release."]} (run update/update! home false)))))))

(deftest update!-unreachable-test
  (with-clone
    (fn [home remote]
      (testing "when the repository cannot be reached, nothing is changed"
        (fs/delete-tree remote)
        (let [{:keys [exit said]} (run update/update! home false)]
          (is (= 1 exit))
          (is (re-find #"could not be reached" (first said))))))))
