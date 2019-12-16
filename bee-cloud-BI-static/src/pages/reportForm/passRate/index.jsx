import React, { Component } from 'react';
import { message } from 'antd';
import { connect } from 'dva';
import { FormItemSelect, FormItemYearPicker } from '@/components/FormWidget';
import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, productsOptions } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemYearPicker
        getFieldDecorator={getFieldDecorator}
        label="年份"
        fieldId="year"
        required={false}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

@connect(({ PassRateModel, ReportFormCommonModel, loading }) => ({
  furnaceOptions: ReportFormCommonModel.furnaceOptions,
  shiftCodeOptions: ReportFormCommonModel.shiftCodeOptions,
  productsOptions: ReportFormCommonModel.productsOptions,

  listSearchParams: PassRateModel.listSearchParams,
  parentTabs: PassRateModel.parentTabs,
  dataList: PassRateModel.dataList,
  columns: PassRateModel.columns,

  loading: loading.effects['PassRateModel/getListEffect'],
}))
class Index extends Component {
  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看合格率', '', '', '']);
    } catch (error) {}

    const defaultTabPagesParams = {
      type: 'pass_rate',
    };

    this.getTabPages(defaultTabPagesParams);
  }

  // 表单查询改变事件
  handleFormChange = field => {
    this.getTableData(field);
  };

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData();
  };

  // tab切换事件
  handleTabChange = reportType => {
    const data = { reportType };
    this.handleFormChange(data);
  };

  // 表格分页事件
  handleTableChange = payload => {
    this.getTableData(payload);
  };

  // 是否能查看当前页面
  getTabPages = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'PassRateModel/getTabPagesEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          this.getSearchOptions();
        }
      },
    });
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/fetchFurnacesShiftCodeEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else if (status === 'ok') {
          const { productsOptions } = data;

          const newlistSearchParams = {
            productId: productsOptions[0].value,
          };

          this.handleFormChange(newlistSearchParams);
        }
      },
    });
  };

  // 获取表格数据
  getTableData = payload => {
    // 表格之前的查询条件
    const { dispatch } = this.props;

    dispatch({
      type: 'PassRateModel/getListEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      loading,
      listSearchParams,
      columns,
      dataList,
      parentTabs,
    } = this.props;

    const searchFormProps = {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      dataDetail: listSearchParams,
      handleSearch: this.handleSearch,
      onChange: this.handleFormChange,
    };

    const selfProps = {
      loading,
      columns,
      data: dataList,
      bordered: true,
      scroll: { x: 2000 },
      handleTabChange: this.handleTabChange,
      handleTableChange: this.handleTableChange,

      parentTabs: null,
      childTabs: null,

      ...searchFormProps,
    };
    if (parentTabs.length === 0) {
      return <h1 style={{ marginLeft: '40%' }}>请联系管理员开启当前页面</h1>;
    }
    return <NewCom {...selfProps} />;
  }
}

export default Index;
