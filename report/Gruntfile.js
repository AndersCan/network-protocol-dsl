module.exports = function (grunt) {
grunt.loadNpmTasks('grunt-latex');
grunt.loadNpmTasks('grunt-contrib-watch');

grunt.initConfig({
  latex: {
    src: ['document.tex']
  },
  watch: {
    files: ['*.tex'],
    tasks: ['latex:src']
  }
});

};
