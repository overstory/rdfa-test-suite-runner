
$(function ()
{
    Highcharts.getOptions().colors = Highcharts.map(Highcharts.getOptions().colors, function(color) {
        return {
            radialGradient: { cx: 0.5, cy: 0.3, r: 0.7 },
            stops: [
                [0, color],
                [1, Highcharts.Color(color).brighten(-0.3).get('rgb')] // darken
            ]
        };
    });

    $('#graph-container').highcharts({

        colors: [ '#00cc00', '#cccc00', '#cc0000' ],

        chart: {
            type: 'pie',
            borderColor: '#66aaaa',
            borderWidth: 1,
            backgroundColor: ''
        },
        title: {
            text: 'Results'
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
            name: 'Result',
            data:
                    [
                        ['Pass', 5 ],
                        ['Fail', 4 ],
                        ['Error', 2 ]
                    ]
    }]
});
});
