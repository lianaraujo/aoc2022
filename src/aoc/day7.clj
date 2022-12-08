(def filename "resources/day7")

(def example "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")


(defn parse-fs
  [input]
  (loop [fs   {}
         path []
         [line & lines] (clojure.string/split input #"\n *")]
    (if-not line
      fs
      (let [[         _     ls      dir  size  file         cd]
            (re-find #"[$] (ls)|dir (.*)|(\d+) (.*)|[$] cd (.*)" line)]
        (cond 
          ls   (recur fs path lines)
          dir  (recur (assoc-in fs (conj path dir)  {})                path lines)
          file (recur (assoc-in fs (conj path file) (parse-long size)) path lines)
          cd   (case cd
                 "/"  (recur fs []         lines)
                 ".." (recur fs (pop path) lines)
                 (recur fs (conj path cd) lines)))))))

(defn path->size
  [fs]
  (let [result (new java.util.HashMap)
        collect (fn collect [path dir]
                  (let [size (transduce
                              (map (fn [[name size-or-dir]]
                                     (if (int? size-or-dir)
                                       size-or-dir
                                       (collect (conj path name) size-or-dir))))
                              + 0 dir)]
                    (. result put path size)
                    size))]
    (collect [] fs)
    (into {} result)))

(defn part1
  [input]
  (let [fs (parse-fs input)
        p->s (path->size fs)]
    (transduce
     (comp
      (map val)
      (filter #(<= % 100000)))
     + 0 p->s)))

(defn part2
  [input]
  (let [fs (parse-fs input)
        p->s (path->size fs)
        used (p->s [])
        debt (- used 40000000)]
    (transduce
     (comp
      (map val)
      (filter #(>= % debt)))
     min
     Long/MAX_VALUE
     p->s)))
