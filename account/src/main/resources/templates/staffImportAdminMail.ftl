<!Doctype html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Email Design</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <link rel="stylesheet" href="dist/css/common.css" type="text/css">
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
                <h1 style="font-size:24px;text-align:center;color:#4a4a4a;font-weight:600;margin:0 0 50px 0;"> Summary of Invites Sent</h1>
                <div style="margin-bottom:30px;">
                    <p class="name" style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:15px;text-transform:capitalize;">Hi <b>${ADMIN_NAME}</b>,</p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;"> You recently imported a file to invite multiple contacts to your YoCoBoard account. Please find a summary of the invites sent:</p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;"> Total Upload: ${TOTAL_COUNT} </p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;"> Successfully Sent : ${SUCCESS_COUNT} </p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;"> Failed to Send :  ${FAILED_COUNT_WITH_MSG} </p>
                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:30px;line-height:22px;"> If you have any questions or require any assistance, contact our support team by sending an email to <a style="color:#f14956;text-decoration:none;">${SUPPORT_MAIL}</a>.</p>
                </div>
                <div>
                    <span style="color:rgba(74,74,74,0.8);font-size:14px;">Happy Time Tracking!</span>
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

