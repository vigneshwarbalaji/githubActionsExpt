<!Doctype html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Email Design</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
</head>
<body style="margin:0;padding: 0;background-color:#fafafa;">
<table style="border-spacing: 0;padding:0;border: 0;width: 750px;margin: 0 auto;font-family: 'Helvetica Neue',sans-serif;box-shadow: 0 0 4px 0 rgba(0,0,0,0.20), 1px 1px 4px 0 rgba(0,0,0,0.20);">
    <thead style="padding: 30px 50px;display: block;">
    <tr>
        <th>
            <a href="https://www.yocoboard.com/"><img src="${DOMAIN_IMAGE}min-images/png/yoco-logo-red2x.png" alt="logo" style="height:50px;width:56px;"></a>
        </th>
    </tr>
    </thead>
    <tbody>
    <tr>
        <td>
            <section style="width:650px;margin:0 auto;background-color:#fff;border-radius: 4px;padding:30px 50px;box-sizing:border-box;">
                <h1 style="font-size:24px;text-align:center;color:#4a4a4a;font-weight:600;margin:0 0 50px 0;">Payroll Request Approved</h1>
                <div style="margin-bottom:30px;">
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:15px;text-transform:capitalize;">Hey ${USER_NAME},</p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;">
                        Your payroll request got approved by
                        <b style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;text-transform:capitalize;"> ${ADMIN_NAME} </b>
                        for the following date(s).
                    </p>
                    <table style="border-spacing:0;padding:0;border-bottom: solid 1px rgba(191, 192, 192, 0.4);font-family:'Helvetica Neue',sans-serif;margin-bottom:30px;">
                        <tr style="width:550px;display:inline-table;padding:10px 0;border-top: solid 1px rgba(191, 192, 192, 0.4);">
                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Date</th>
                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">In Time</th>
                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Out Time</th>
                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Duration</th>
                        </tr>
                        ${SESSION_DETAILS}
                        <tr style="width:550px;display:inline-table;padding:10px 0;border-top: solid 1px rgba(191, 192, 192, 0.4);">
                            <th style="text-align:left;width:75%;font-size:14px;font-weight:600;color:#4a4a4a;">Total Duration</th>
                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">${TOTAL_HOURS}</th>
                        </tr>
                    </table>
                </div>
                <div>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:25px;">If you have any questions, please feel free to contact
                        <a style="color:#f14956;text-decoration:none;">${SUPPORT_MAIL}</a>
                    </p>
                    <span style="color:rgba(74,74,74,0.8);font-size:14px;">Thank You</span>
                    <cite style="color:rgba(74,74,74,0.8);font-size:14px;font-weight:600;font-style:normal;display:block;margin-top:15px;">The YoCoBoard Team</cite>
                </div>
            </section>
        </td>
    </tr>
    </tbody>
    <tfoot style="padding:30px 0 50px 0;display:inline-block;text-align:center;width:100%;">
    <tr style="width:100%;text-align:center;display:block;">
        <td style="display:block;">
            <div style="">
                <a href="https://www.facebook.com/yocoboard"><img src="${DOMAIN_IMAGE}min-images/png/fb2x.png" style="width:6px;height:10px;display:inline-block;margin-right:20px;"></a>
                <a href="https://twitter.com/yocoboard"><img src="${DOMAIN_IMAGE}min-images/png/twitter2x.png" style="width:10px;height:8px;display:inline-block;margin-right:20px;"></a>
                <a href="https://www.linkedin.com/in/yocoboard"><img src="${DOMAIN_IMAGE}min-images/png/linkedin2x.png" style="width:10px;height:10px;display:inline-block;"></a>
            </div>
        </td>
    </tr>
    <tr style="width:100%;text-align:center;display:block;">
        <td style="display:block;">
            <cite style="color:#4a4a4a; font-size:8px;font-style:normal;opacity:0.5;">&copy; ${CURRENT_YEAR} YoCoBoard</cite>
        </td>
    </tr>
    </tfoot>
</table>
</body>
</html>