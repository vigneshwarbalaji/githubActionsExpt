<!Doctype html>
            <html>
             <head>
              <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
              <title>Adjustment Deleted Email</title>
              <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
            </head>
            <body style="margin:0;padding: 0;background-color:#fafafa;">
                <table style="border-spacing: 0;padding:0;border: 0;width: 750px;margin: 0 auto;font-family: 'Helvetica Neue',sans-serif;box-shadow: 0 0 4px 0 rgba(0,0,0,0.20), 1px 1px 4px 0 rgba(0,0,0,0.20);">
                    <thead style="padding: 30px 50px;display: block;">
                        <tr>
                            <th>
                                <img src="https://my.yocoboard.com/min-images/png/yoco-logo-red2x.png" alt="logo" style="height:50px;width:56px;">
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                    <tr>
                        <td>
                            <section style="width:650px;margin:0 auto;background-color:#fff;border-radius: 4px;padding:30px 50px;box-sizing:border-box;">
                                <h1 style="font-size:24px;text-align:center;color:#4a4a4a;font-weight:600;margin:0 0 50px 0;">Adjustments ${ACTION}</h1>
                                <div style="margin-bottom:30px;">
                                    <p class="name" style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:15px;text-transform:capitalize;">Hey ${USER_NAME},</p>
                                    <p style="color:rgba(74,74,74, 0.8);font-size:14px;margin-bottom:30px;line-height:22px;">Your adjustment requests mentioned below got ${ACTION} by <b>${ADMIN_NAME}</b>${MESSAGE}.</p>
                                    <table style="border-spacing:0;padding:0;border-bottom: solid 1px rgba(191, 192, 192, 0.4);font-family:'Helvetica Neue',sans-serif;margin-bottom:30px;">
                                        <tr style="width:550px;display:inline-table;padding:10px 0;border-top: solid 1px rgba(191, 192, 192, 0.4);">
                                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Date</th>
                                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">In Time</th>
                                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Out Time</th>
                                            <th style="text-align:left;width:25%;font-size:14px;font-weight:600;color:#4a4a4a;">Total</th>
                                        </tr>
                                        ${ADJUSTMENTS_DELETED_CONTENT}
                                    </table>
                                    <a href="${REDIRECT_URL}" style="background-color:#bf001b;background-image:linear-gradient(to right, #f34b58, #bf001b);font-size:14px;color:#fff;padding:8px 20px;border:none;border-radius:100px;text-decoration:none;">Check Adjustments</a>
                                </div>
                                <div>
                                    <p style="color:rgba(74,74,74,0.8);font-size:14px;margin-bottom:25px;">If you have any questions, please feel free to contact <a style="color:#f14956;text-decoration:none;">${ADMIN_MAIL}</a></p>
                                    <span style="color:rgba(74,74,74,0.8);font-size:14px;">Thank you</span>
                                    <cite style="color:rgba(74,74,74,0.8);font-size:14px;font-weight:600;font-style:normal;display:block;margin-top:15px;">The YoCoBoard Team</cite>
                                </div>
                            </section>
                        </td>
                    </tr>
                    </tbody>
                    <tfoot style="padding:30px 0 50px 0;display:inline-block;text-align:center;width:100%;">
                        <tr style="width:100%;text-align:center;display:block;">
                            <td style="display:block;">
                                <div>
                                    <a href="https://www.facebook.com/yocoboard"><img src="https://my.yocoboard.com/min-images/png/fb2x.png" alt="facebook" style="width:6px;height:10px;display:inline-block;margin-right:20px;"></a>
                                    <a href="https://twitter.com/yocoboard"><img src="https://my.yocoboard.com/min-images/png/twitter2x.png" alt="twitter" style="width:10px;height:8px;display:inline-block;margin-right:20px;"></a>
                                    <a href="https://www.linkedin.com/in/yocoboard"><img src="https://my.yocoboard.com/min-images/png/linkedin2x.png" alt="linkedin" style="width:10px;height:10px;display:inline-block;"></a>
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