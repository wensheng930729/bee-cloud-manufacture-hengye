import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

const prefix = `${api_factory_prefix}`;

// 获取列名
/*
businessType:
reportType:

productId

*/
export async function getReportFormFields(params) {
  return request(`${prefix}/reportForm/getReportFormFields?${stringify(params)}`);
}

// 根据类别查询仓库列表
/*
rq:{
  types:[],
}
sysToken:''
*/
export async function getRepositoryListByType(params = {}) {
  return request(`${prefix}/configRepository/getRepositoryListByType`, {
    method: 'POST',
    body: params,
  });
}
/*
获取产品列表
{sysToken:'' }
*/
export async function getProducts(params) {
  return request(`${prefix}/reportForm/getProducts?${stringify(params)}`);
}

/*
获取 报表-库存-现存明细表
{
  currentPage:'',
  endTime:'',
  orderStage:'',
  pageSize:'',
  productId:'',
  searchCount:'',
  startTime:'',
  storageId:'',

 }
*/
export async function getExistingDeatilsReportForm(params) {
  return request(`${prefix}/reportForm/getExistingDeatilsReportForm?${stringify(params)}`);
}
/*
报表-库存-采购入库表  参数同上
*/
export async function getBuySendStorageReportForm(params) {
  return request(`${prefix}/reportForm/getBuySendStorageReportForm?${stringify(params)}`);
}
/*
报表-库存-原料日报表
{
  currentPage:'',
  dayTime:'',
  orderStage:'',
  pageSize:'',
  productId:'',
  searchCount:'',

}
*/
export async function getRawMaterialReportForm(params) {
  return request(`${prefix}/reportForm/getRawMaterialReportForm?${stringify(params)}`);
}

/*
报表-库存-报表-产成品入库报表
{
  currentPage:'',
  depositoryId:'仓库id',
  endTime:'',
  orderStage:'',
  pageSize:'',
  productId:'',
  searchCount:'',
  startTime:'',


}
*/
export async function getProductWarehouseReportForm(params) {
  return request(`${prefix}/reportForm/getProductWarehouseReportForm?${stringify(params)}`);
}

/*
报表-库存-成品出库表
{
  currentPage:'',
  endTime:'',
  orderStage:'',
  pageSize:'',
  productId:'',
  searchCount:'',
  startTime:'',
  storageId:'',
  sysToken :'',
}
*/
export async function getProductOutStorageReportForm(params) {
  return request(`${prefix}/reportForm/getProductOutStorageReportForm?${stringify(params)}`);
}
// 获取产品类别
export async function getProductCategories(params) {
  return request(`${prefix}/reportForm/getProductCategories?${stringify(params)}`);
}
// 质检报表
export async function getQualityTestReportForm(params) {
  return request(`${prefix}/reportForm/getQualityTestReportForm?${stringify(params)}`);
}
//
export async function getBuyReportForm(params) {
  return request(`${prefix}/reportForm/getBuyReportForm?${stringify(params)}`);
}
// 获取所有炉号
export async function getFurnaces(params) {
  return request(`${prefix}/reportForm/getFurnaces?${stringify(params)}`);
}
// 获取班次
export async function listConfigCodeByType(params) {
  return request(`${prefix}/configCode/listConfigCodeByType?${stringify(params)}`);
}
// 物流报表
export async function getSaleLogisticsReportForm(params) {
  // 销售运输台账
  return request(`${prefix}/reportForm/getSaleLogisticsReportForm?${stringify(params)}`);
}
export async function getBuyLogisticsReportForm(params) {
  // 采购运输台账
  return request(`${prefix}/reportForm/getBuyLogisticsReportForm?${stringify(params)}`);
}

// 获取产量分析列表
export async function getYieldAnalysisReportForm(params) {
  return request(`${prefix}/reportForm/getYieldAnalysisReportForm?${stringify(params)}`);
}

// 获取合格率 列表
export async function getPassRateReportForm(params) {
  return request(`${prefix}/reportForm/getPassRateReportForm?${stringify(params)}`);
}

// 产量消耗分析表
export async function getOutputStatisticsReportForm(params) {
  return request(`${prefix}/reportForm/getOutputStatisticsReportForm?${stringify(params)}`);
}

// 销售报表
export async function getSaleReportForm(params) {
  return request(`${prefix}/reportForm/getSaleReportForm?${stringify(params)}`);
}
