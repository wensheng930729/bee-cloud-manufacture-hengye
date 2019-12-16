package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO;
import com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO;
import com.bee.platform.cloud.si.manufacture.dto.InventoryTypeDTO;
import com.bee.platform.cloud.si.manufacture.dto.StockInventoryListDTO;
import com.bee.platform.cloud.si.manufacture.entity.StockInventory;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.InventoryRQ;
import com.bee.platform.cloud.si.manufacture.rq.InventorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.StockInventoryRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 库存盘点主表 服务类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
public interface StockInventoryService extends IService<StockInventory> {

    /**
     * @notes: 盘点单类型查询
     * @Author: junyang.li
     * @Date: 17:06 2019/11/25
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.InventoryTypeDTO>>
     */
    ResponseResult<List<InventoryTypeDTO>> getInventoryType();
    /**
     * @notes: 根据盘点分类获得下拉列表详细
     * @Author: junyang.li
     * @Date: 9:40 2019/11/26
     * @param inventoryTypeCode : 盘点类型code
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDesc(AuthPlatformUserInfo userInfo,
                                                                    Integer inventoryTypeCode);
    /**
     * @notes:  创建库存盘点单，并返回待盘点数据
     * @Author: junyang.li
     * @Date: 13:31 2019/11/26
     * @param userInfo : 当前操作人
     * @param rq : 待查询的类别参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryDetailDTO>
     */
    ResponseResult<InventoryOrderDTO> createInventoryOrder(AuthPlatformUserInfo userInfo, InventoryRQ rq);

    /**
     * @notes: 保存盘点单调整后的数据
     * @Author: junyang.li
     * @Date: 9:26 2019/11/27
     * @param userInfo : 当前用户
     * @param rq : 调整后数据
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> saveInventoryInfo(AuthPlatformUserInfo userInfo, StockInventoryRQ rq);
    /**
     * @notes: 盘点单列表查询
     * @Author: junyang.li
     * @Date: 14:33 2019/11/27
     * @param userInfo : 当前操作人
     * @param rq : 查询参数
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.si.manufacture.dto.StockInventoryListDTO>>
     */
    ResponseResult<List<StockInventoryListDTO>> getInventoryInfoList(AuthPlatformUserInfo userInfo, InventorySearchRQ rq);
    /**
     * @notes: 根据盘点单号查询盘点单详细
     * @Author: junyang.li
     * @Date: 15:31 2019/11/27
     * @param userInfo : 当前操作人
     * @param inventoryOrderId : 盘点单号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO>
     */
    ResponseResult<InventoryOrderDTO> getInventoryInfoById(AuthPlatformUserInfo userInfo,String inventoryOrderId);
}
