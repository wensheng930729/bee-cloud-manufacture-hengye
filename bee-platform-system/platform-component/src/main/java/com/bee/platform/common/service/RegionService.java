package com.bee.platform.common.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.Region;
import com.bee.platform.common.entity.RegionInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.TreeNode;

import java.util.List;
import java.util.Map;

public interface RegionService extends IService<Region> {

    /**
     * 查询父级id相同的地区
     * @param pid
     * @return
     */
    List<Region> findRegionByParentId(int pid);

    /**
     * 根据id查询地区
     * @param id
     * @return
     */
    RegionInfo findRegionById(Integer id);
    /**
     * @notes 用户信息根据RegionId返回RegionDTO对象
     * @Author junyang.li
     * @Date 13:50 2019/3/6
     **/
    RegionDTO selectRegion(String regionId);



    /**
     * 查询所有地区信息
     * @return 树形结构地区信息
     */
    List<TreeNode> getAllRegion();

    /**
     * 查询所有的省级区域
     * @return
     */
    List<Region> getAllProvince();

    String assembleFullAddress(Integer districtId, String street);

    ResponseResult<Map<String, Object>> findAllRegionById(Integer id);

}
