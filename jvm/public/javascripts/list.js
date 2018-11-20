$('span[data-latex]').each(function() {
    katex.render(this.getAttribute('data-latex'), this, {
        throwOnError: false
    });
});
