package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.bee.platform.cloud.si.manufacture.dto.SaleBackAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleResultDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleBackAssayResult;
import com.bee.platform.cloud.si.manufacture.rq.SaleBackAssayResultSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.SampleAssayResultSaveRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 销售车次化验结果反馈表 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-27
 */
public interface SaleBackAssayResultService extends IService<SaleBackAssayResult> {

    /**
     * 保存样品化验结果
     *
     * @param rq
     * @return
     */
    public ResponseResult<ResCodeEnum> saveAssayResult(SaleBackAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 查询化验结果
     * @param carrierTransportDetailId
     * @return
     */
	public ResponseResult<List<SaleBackAssayResultDTO>> getAssayResult(String carrierTransportDetailId);

}
