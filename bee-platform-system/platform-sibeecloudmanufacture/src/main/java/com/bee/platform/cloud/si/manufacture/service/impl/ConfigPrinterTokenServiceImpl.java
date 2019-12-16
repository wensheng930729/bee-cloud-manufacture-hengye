package com.bee.platform.cloud.si.manufacture.service.impl;

import com.bee.platform.cloud.si.manufacture.constants.enums.EnumPrinter;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.PrintRq;
import com.bee.platform.cloud.si.manufacture.service.ConfigPrinterTokenService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.utils.MD5;
import com.bee.platform.cloud.si.manufacture.utils.RequestUtils;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import net.sf.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 打印机码表 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-30
 */
@Slf4j
@Service
public class ConfigPrinterTokenServiceImpl extends ServiceImpl<ConfigPrinterTokenMapper, ConfigPrinterToken> implements ConfigPrinterTokenService {


    @Autowired
    private ConfigPrinterUrlMapper urlMapper;
    @Autowired
    private ConfigPrinterTokenMapper tokenMapper;
    @Autowired
    private ConfigProductMapper productMapper;
    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;
    @Autowired
    private SaleWeightMachineMapper saleWeightMachineMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult printPoundSheet(AuthPlatformUserInfo userInfo, PrintRq rq) {
        ConfigPrinterToken printerToken = tokenMapper.selectOne(new ConfigPrinterToken()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setDeviceId(rq.getDeviceId()));
        if(ObjectUtils.isEmpty(printerToken)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_URL_NOT_TOKEN);
        }
        if(ObjectUtils.isEmpty(printerToken.getAccessToken())){
            //accessToken为空时 重新获取token 执行打印
            reSetTokenAndPrint(userInfo,printerToken,rq);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //查询打印机状态
        String printerStatus = getPrinterStatus(printerToken);
        log.info("打印机状态码："+printerStatus);
        switch (printerStatus){
            case "0":
                //执行打印
                String executePrint = executePrint(userInfo,printerToken, rq);
                JSONObject jsonPrintFirst = JSONObject.fromObject(executePrint);
                String codePrintFirst  = jsonPrintFirst .getString("return_code");
                if(!EnumPrinter.JOLI_MARK_STATUS.SUCCESS.getKey().equals(codePrintFirst )){
                    throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_SYSTEM_ERROR);
                }
                break;
            case "10102":
            case "10103":
                //重新获取token 执行打印
                reSetTokenAndPrint(userInfo,printerToken,rq);
                break;
            default:
                throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_SYSTEM_ERROR);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 重新获取token并打印
     *
     */
    private void reSetTokenAndPrint(AuthPlatformUserInfo userInfo, ConfigPrinterToken printerToken, PrintRq rq){
        //重新获取token
        String result = getToken(printerToken);
        JSONObject jsonToken = JSONObject.fromObject(result);
        String returnCode = jsonToken.getString("return_code");
        if(EnumPrinter.JOLI_MARK_STATUS.SUCCESS.getKey().equals(returnCode)){
            //获取token成功
            String returnData = jsonToken.getString("return_data");
            JSONObject returnDataObject = JSONObject.fromObject(returnData);
            String token = returnDataObject.getString("access_token");
            //更新token
            printerToken.setAccessToken(token);
            tokenMapper.updateById(printerToken);
            //执行打印
            String executePrintSecond = executePrint(userInfo,printerToken, rq);
            JSONObject jsonPrintSecond = JSONObject.fromObject(executePrintSecond);
            String codePrintSecond = jsonPrintSecond.getString("return_code");
            if(!EnumPrinter.JOLI_MARK_STATUS.SUCCESS.getKey().equals(codePrintSecond)){
                throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_SYSTEM_ERROR);
            }
        }
    }


    /**
     * 获取token
     * @param printerToken
     * @return
     */
    private String getToken(ConfigPrinterToken printerToken){
        ConfigPrinterUrl configTokenUrl = urlMapper.selectOne(new ConfigPrinterUrl().setType(EnumPrinter.URL_TYPE.TOKEN.getKey()));
        if(ObjectUtils.isEmpty(configTokenUrl)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_URL_NOT_TOKEN);
        }
        // MD5签名
        String appId = printerToken.getAppId();
        Long time = System.currentTimeMillis() / 1000;
        String base = "app_id="+ appId +"&sign_type=MD5&time_stamp="+time+"&key=" +printerToken.getAppKey();
        String param = "app_id=" + appId +
                "&time_stamp=" + time + // 时间戳
                "&sign=" + MD5.encrypt(base) +
                "&sign_type=MD5";
        //发送获取token请求
        String result = RequestUtils.sendGetPrint(configTokenUrl.getUrl(), param);
        return result;
    }

    /**
     * 查询打印机状态
     * @param printerToken
     * @return
     */
    private String getPrinterStatus(ConfigPrinterToken printerToken){
        ConfigPrinterUrl configStatusUrl = urlMapper.selectOne(new ConfigPrinterUrl().setType(EnumPrinter.URL_TYPE.STATUS.getKey()));
        if(ObjectUtils.isEmpty(configStatusUrl)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_URL_NOT_FOUND);
        }
        String statusUrl = configStatusUrl.getUrl();
        System.out.println(statusUrl);
        //查询打印机状态
        String param = "app_id="+printerToken.getAppId() +
                "&device_id="+printerToken.getPrinterId() +
                "&access_token="+printerToken.getAccessToken();
        String result = RequestUtils.sendGetPrint(statusUrl, param);
        JSONObject jsonObject = JSONObject.fromObject(result);
        String returnCode = jsonObject.getString("return_code");
        return returnCode;
    }

    /**
     * 执行打印
     * @param printerToken
     * @param rq
     * @return
     */
    private String executePrint(AuthPlatformUserInfo userInfo,ConfigPrinterToken printerToken,PrintRq rq){
        String unit = "";
        //查询产品单位
        ConfigProduct product = productMapper.selectOne(new ConfigProduct()
                .setId(rq.getProductId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                .setDeleted(EnumCommon.LogicStatus.DELETED.getKey()));
        if(!ObjectUtils.isEmpty(product)){
            unit = product.getUnitValue();
        }
        ConfigPrinterUrl configPrintUrl = urlMapper.selectOne(new ConfigPrinterUrl().setType(EnumPrinter.URL_TYPE.PRINT.getKey()));
        if(ObjectUtils.isEmpty(configPrintUrl)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PRINTER_URL_NOT_FOUND);
        }
        //打印参数组装
        String driver = ObjectUtils.isEmpty(rq.getDriver())?"":rq.getDriver();
        String weighMan = "";
        Date weighingTime = null;
        String remark = "";
        String unitType;
        if(EnumWeightMachine.WeightType.BUY.getValue().equals(rq.getType())){
            BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine()
                    .setMachineId(rq.getMachineId())
                    .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
            if(!ObjectUtils.isEmpty(buyWeightMachine)){
                weighMan = buyWeightMachine.getWeightMan();
                remark = buyWeightMachine.getRemark();
                Date weighingTimeBuy = buyWeightMachine.getWeighingTime();
                weighingTime = ObjectUtils.isEmpty(weighingTimeBuy)?new Date():weighingTimeBuy;
            }
            //模板：发货单位
            unitType = EnumPrinter.TEMPLATE_FIRST.UNIT_TYPE_BUY.getValue();
        }else if(EnumWeightMachine.WeightType.SALE.getValue().equals(rq.getType())){
            SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(new SaleWeightMachine()
                    .setMachineId(rq.getMachineId())
                    .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
            if(!ObjectUtils.isEmpty(saleWeightMachine)){
                remark = saleWeightMachine.getRemark();
                weighMan = saleWeightMachine.getWeightMan();
                Date weighingTimeSale = saleWeightMachine.getWeighingTime();
                weighingTime = ObjectUtils.isEmpty(weighingTimeSale)?new Date():weighingTimeSale;
            }
            //模板：收货单位
            unitType = EnumPrinter.TEMPLATE_FIRST.UNIT_TYPE_SALE.getValue();
        }else{
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER,ExceptionMessageEnum.PRINTER_TYPE_ERROR);
        }
        //采购榜单中毛重公式：进厂重量+（-扣重）\ 销售榜单中毛重公式：出厂重量+（-扣重）
        BigDecimal deductWeight = ObjectUtils.isEmpty(rq.getDeductWeight())?BigDecimal.ZERO:rq.getDeductWeight();
        BigDecimal inFactoryWeight = EnumWeightMachine.WeightType.BUY.getValue()
                .equals(rq.getType())?rq.getInFactoryWeight().subtract(deductWeight):rq.getOutFactoryWeight().subtract(deductWeight);
        BigDecimal outFactoryWeight = EnumWeightMachine.WeightType.BUY.getValue()
                .equals(rq.getType())?rq.getOutFactoryWeight():rq.getInFactoryWeight();
        //执行打印
        String str = "app_id="+printerToken.getAppId()+
                "&access_token="+printerToken.getAccessToken()+
                "&device_ids="+printerToken.getPrinterId()+
                "&template_id="+printerToken.getTemplateId() +
                "&copies=1" +
                "&cus_orderid="+printerToken.getId() +
                "&paper_type=1" +
                "&time_out=600"+
                "&bill_content={"
                + "\"orgName\":\""+userInfo.getOrg_name()+"\","
                + "\"unitType\":\""+unitType+"\","
                + "\"unitTypeName\":\""+rq.getDeliveryCompany()+"\","
                + "\"driver\":\""+driver+"\","
                + "\"unit\":\""+unit+"\","
                + "\"productName\":\""+rq.getProductName()+"\","
                + "\"origin\":\"\","
                + "\"trainNum\":\""+rq.getTrainNumber()+"\","
                + "\"inFactoryWeight\":\""+inFactoryWeight.setScale(2, BigDecimal.ROUND_HALF_UP)+"\","
                + "\"outFactoryWeight\":\""+outFactoryWeight.setScale(2, BigDecimal.ROUND_HALF_UP)+"\","
                + "\"netWeight\":\""+rq.getNetWeight().setScale(2, BigDecimal.ROUND_HALF_UP)+"\","
                + "\"materialUnit\":\"\","
                + "\"weighMan\":\""+weighMan+"\","
                + "\"weighingTime\":\""+DateUtils.format(weighingTime,DateUtils.Y_M_D_H_M_S)+"\","
                + "\"remark\":\""+remark+"\"}";
        String json = RequestUtils.sendPostPrint(configPrintUrl.getUrl(), str);
        log.info(json);
        return json;
    }

}
