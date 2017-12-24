$('body').html('');
$.each(index_data,function(i, value)
{
    var id = value[0],
        url = 'https://' + app_config.domain+user.run_sessions_path+value[0] + '.gpx',
        filename = 'RUNTASTIC-' + value[1] + '-' + value[0] + '.gpx';

    $.ajax({
        url: 'https://' + app_config.domain + user.run_sessions_path + id + '.gpx',
        success: function(data, textStatus, jqXHR)
        {
            if(textStatus == 'success')
            {
                $('<a/>', {
                    'href' : 'data:text/plain;charset=utf-8,' + encodeURIComponent(jqXHR.responseText),
                    'download' : filename,
                    'id' : id
                }).html(filename)
                .before(i + '. Downloaded: ')
                .after('<br/>')
                .prependTo('body');

                $('#' + id)[0].click();
            }
            else
            {
                console.log(textStatus);
            }
        },
        dataType: 'xml',
        beforeSend: function(xhr){
            xhr.setRequestHeader('X-Requested-With', ' ');
        },
    });
});