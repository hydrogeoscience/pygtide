from numpy.distutils.core import Extension, setup

if __name__ == "__main__":
    setup(
        name = "pygtide",
		version = '0.5',
		description = 'PyGTide: A Python module and wrapper for ETERNA PREDICT to compute gravitational tides on Earth',
		author = 'Gabriel C. Rau',
		author_email = 'gabriel@hydrogeo.science',
		url = 'https://github.com/hydrogeoscience/pygtide',
		project_urls = {
			'Author': 'https://hydrogeo.science',
		},
		py_modules = ["pygtide", "test_pygtide", "update_commdat"],
        ext_modules = [Extension(
			"etpred", ["etpred/etpred.f90"], 
			extra_link_args = ["-static", "-static-libgfortran", "-static-libgcc", "-Wno-tabs", "-floop-nest-optimize"],
			include_dirs = ['commdat/']
		)],
		packages = ["pygtide"],
		include_package_data = True
    )