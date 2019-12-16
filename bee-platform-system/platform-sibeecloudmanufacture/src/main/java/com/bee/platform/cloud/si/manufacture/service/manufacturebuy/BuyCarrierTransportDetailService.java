package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyCarrierTransportDetail;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 承运方运输详情表(采购) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyCarrierTransportDetailService extends IService<BuyCarrierTransportDetail> {

    /**
     * 根据承运方运输段ID查询承运方运输详情车次信息
     * @param carrierTransportId
     * @return
     */
    List<BuyTransportDetailDTO> getTransportDetailByTransportId(String carrierTransportId);

    /**
     * 更新车次信息
     * @param transportDetailDTO
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveTransportDetailByTransport(BuyTransportDetailDTO transportDetailDTO, AuthPlatformUserInfo userInfo);

}
