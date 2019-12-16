package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleSample;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 销售取样表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-26
 */
public interface SaleSampleService extends IService<SaleSample> {

    /**
     * 保存取样信息
     */
    public ResponseResult<ResCodeEnum> saveSample(SaveSampleSaleRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 完成取样
     *
     * @param rq
     * @return
     */
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleSaleRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员开始化验样品
     */
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo);

    /**
     * 保存样品化验结果
     *
     * @param rq
     * @return
     */
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 化验人员弃用样品
     */
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据样品code获取样品详情
     *
     * @param o
     * @param sampleCode
     * @return
     */
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode);

    /**
     * 手动输入销售取样的时候 在出库的时候设置合同业务id
     *
     * @param tonCodeList
     * @return
     */
    @Deprecated
    boolean sampleSaleSetContractBusinessId(List<String> tonCodeList);

}
