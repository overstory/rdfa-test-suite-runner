
$(function ()
{
    Highcharts.getOptions().colors = Highcharts.map(['#00cc00', '#cccc00', '#cc0000', '#aaaaaa'], function(color) {
        return {
            radialGradient: { cx: 0.5, cy: 0.3, r: 0.7 },

            stops: [
                [0, color],
                [1, Highcharts.Color(color).brighten(-0.4).get('rgb')] // darken
            ]
        };
    });

    $('#graph-container').highcharts({

        chart: {
            type: 'pie',
            borderColor: '#66aaaa',
            borderWidth: 3,
            backgroundColor: ''
        },

        title: {
            text: 'RDFa Test Suite Results'
        },

        plotOptions: {
            pie: {
                allowPointSelect: true,
                cursor: 'pointer',
                dataLabels: {
                    enabled: true,
                    color: '#000000',
                    connectorColor: '#000000',
                    format: '<b>{point.name}</b>: {point.percentage:.1f} %'
                }
            }
        },

        series: [{
            type: 'pie',
            name: 'Count'
        }]
    });

});
