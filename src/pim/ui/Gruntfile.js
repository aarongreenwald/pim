module.exports = function(grunt) {

  grunt.initConfig({
    jshint: {
      files: ['[!bower_components|dist]**/*.js'],
      options: {
        asi: true, //no semicolons! yay        
        supernew: true //allow 'new function'
      }
    },
    concat : {
      options: {
        verbose: true,
        separator: '\n;\n',
      },  
      dist: {
        src: ['**/*.module.js',
              '**/*.routes.js', 
              '**/*.service.js', 
              '**/*.directive.js', 
              '**/*.controller.js',
              '[!bower_components|dist]**/*.js'],
        dest: 'dist/pim.js'
      },      
    },
    uglify : {      
      'dist/pim.min.js': ['dist/pim.js']      
    },
    watch: {
      files: ['./**/*.js', '!./dist/**/*', '!./bower_components/**/*'],
      tasks: ['jshint', 'build']
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jshint')
  grunt.loadNpmTasks('grunt-contrib-watch')
  grunt.loadNpmTasks('grunt-contrib-uglify')
  grunt.loadNpmTasks('grunt-contrib-concat')

  grunt.registerTask('default', ['jshint', 'build'])
  grunt.registerTask('develop', ['jshint', 'build', 'watch'])
  grunt.registerTask('build', ['concat', 'uglify'])

};
