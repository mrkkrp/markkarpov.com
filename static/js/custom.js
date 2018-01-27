document.addEventListener("DOMContentLoaded", function(event) {
    anchors.options = {
        placement: 'right',
        visible: 'always',
        class: 'anchor'
    };
    anchors.add('h2, h3, h4');
});

$(document).ready(function () {
    $(".protected-email").each(function (i,v) {
        var item = $(this);
        var email = item.data('email');
        item.attr('href', 'mailto:' + email);
        item.html(email);
    });
});
