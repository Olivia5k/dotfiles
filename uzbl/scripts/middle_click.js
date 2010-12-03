(function() {
	window.addEventListener("click", function(e) {
		if (e.button === 1) {
			var new_uri = e.srcElement.href;
			if (new_uri) {
				e.stopPropagation();
				e.preventDefault();
				window.open(new_uri);
			}
		}
	}, false);
})();
